// Copyright (C) 2011 Oliver Schulz <oliver.schulz@tu-dortmund.de>

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


package daqcore.io


import akka.actor._, akka.actor.Actor._, akka.dispatch.Future
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}

import daqcore.actors._
import daqcore.util._


class VCPIOClientSrv(val stream: ByteStreamIO) extends CascadableServer with QueueingServer {
  override def profiles = super.profiles.+[RawMsgIO]

  import VCPIOClientSrv._
  import RawMsgInput._

  val recvQueue = new ReplyQueue
  
  protected var inputHeader: Option[Header] = None
  val headerBuf = ByteSeqBuilder(8)
  val dataBuf = ByteSeqBuilder()
  var toRead = 0
  
  protected def nextMsg() {
    inputHeader = None
    headerBuf.clear()
    toRead = 8
  }
  
  nextMsg()

  override def init() = {
    super.init()
    clientLinkTo(stream.srv)
    atShutdown { stream.close() }
    stream.triggerRecv()
  }
  
  override def onServerExit(server: ActorRef, reason: Option[Throwable]) = {
    if ((server == stream.srv) && (reason == None)) self.stop()
    else super.onServerExit(server, reason)
  }

  
  def srvProcessInput(bytes: ByteSeq) = {
    stream.triggerRecv()
    // log.trace("Processing input packet of %s byte(s)".format(bytes.length))
    // log.trace("Received: " + loggable((bytes map hex) mkString " "))

    val from = bytes.iterator
    while (from.hasNext) {
      inputHeader match {
        case None => {
          while ((toRead > 0) && from.hasNext) { headerBuf += from.next(); toRead -= 1 }
          if (toRead == 0) {
            implicit val encoding = BigEndian
            val input = headerBuf.result().iterator
            val opByte = input.getByte()
            val operation = Operation (
              data = (opByte & (1 << 7)) > 0,
              srq = (opByte & (1 << 3)) > 0,
              eoi = (opByte & (1 << 0)) > 0
            )
            val version = input.getByte() //!! currently ignored
            val seqno = input.getByte() //!! currently ignored
            val spare = input.getByte()
            val dataLen = input.getInt()
            
            inputHeader = Some(Header(version = version, seqno = seqno, operation = operation))

            if (dataLen > 0) assert ( operation.data )
            toRead = dataLen
            log.trace("Received VICPIO header: %s, %d data bytes ".format(inputHeader.get, dataLen))
          }
        }
        case Some(hdr) => {
          val n = toRead min from.len
          val a = Array.ofDim[Byte](n)
          from.copyToArray(a, 0, n)
          toRead -= n
          if (hdr.operation.data) dataBuf ++= a
          if (toRead == 0) {
            if (hdr.operation.data && hdr.operation.eoi) {
              val recvData = dataBuf.result()
              log.trace("Received complete data message of size %s".format(recvData.length))
              recvQueue.addReply(Received(recvData)){}
            }
            nextMsg()
          }
        }
      }
    }
  }


  protected def srvSend(data: ByteSeq): Unit = if (!data.isEmpty) {
    def bv(bit: Byte, v: Boolean) = (if (v) (1 << bit) else 0).toByte
    
    val op = Operation(data = true, eoi = true)
    val hdr = Header(version = 1, seqno = 0, operation = op)
    val opByte = ( bv(7, op.data) | bv(3, op.srq) | bv(0, op.eoi) ).toByte
  
    implicit val encoding = BigEndian
    val bld = ByteSeqBuilder()
    
    bld += opByte
    bld += hdr.version
    bld += hdr.seqno
    bld += 0.toByte // spare
    bld.putInt(data.length)
    bld ++= data
    val msg = bld.result()

    log.trace("Sending message: " + loggable((msg map hex) mkString " "))
    stream.send(msg)
  }

  protected def srvFlush(): Unit = stream.flush()
 
  override def serve = super.serve orElse {
    case RawMsgInput.Recv() => {
      recvQueue addTarget replyTarget
    }

    case ByteStreamInput.Received(data) => { srvProcessInput(data) }

    case RawMsgOutput.Send(data) => srvSend(data)

    case RawMsgOutput.Flush() => srvFlush()
  }
}


object VCPIOClientSrv {
  case class Operation(
    data: Boolean = false,
    srq: Boolean = false,
    eoi: Boolean = false
  )

  case class Header(
    version: Byte,
    seqno: Byte,
    operation: Operation
  )
}


object VCPIOClient {
  def apply(stream: ByteStreamIO, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): RawMsgIO = {
    new ServerProxy(sv.linkStart(actorOf(new VCPIOClientSrv(stream)), lc)) with RawMsgIO
  }
}
