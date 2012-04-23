// Copyright (C) 2011-2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


import akka.actor._

import daqcore.util._
import daqcore.actors._, daqcore.actors.TypedActorTraits._


trait VICPLink extends RawMsgIO with CloseableTA {
}


object VICPLink extends IOResourceCompanion[VICPLink] {
  def newInstance = {
    case HostURL("vicp", host, port) =>
      () => new VICPLinkImpl(HostURL("tcp", host, port).toString)
  }
  
  def apply(stream: ByteStreamIO)(implicit rf: ActorRefFactory): VICPLink =
    typedActorOf[VICPLink](new VICPLinkImpl(actorRef(stream).path.toString))


  case class VICPMsg (
    seqno: Byte = 0,
    srq: Boolean = false,
    eoi: Boolean = false,
    data: Option[ByteString] = None
  ) {
    def version: Byte = 1
  }


  object VICPCodec extends Codec[VICPMsg, VICPMsg] {
    implicit def encoding = BigEndian

    private def encFct(out: ByteStringBuilder, msg: VICPMsg): Unit = {
      def bv(bit: Byte, v: Boolean) = (if (v) (1 << bit) else 0).toByte
      
      val opByte = ( bv(0, msg.eoi) | bv(3, msg.srq) | bv(7, !msg.data.isEmpty) ).toByte
    
      val bld = ByteSeqBuilder()
      
      bld.putByte(opByte)
      bld.putByte(msg.version)
      bld.putByte(msg.seqno)
      bld.putByte(0.toByte) // spare
      msg.data match {
        case Some(data) => {
          bld.putInt(data.length)
          bld ++= data
        }
        case None => bld.putInt(0)
      }

      out ++= bld.result.toByteString
    }
    
    val enc = encFct(_, _)
    
    val dec = for {
      rawHeader <- IO take 8
      input = rawHeader.toByteSeq.iterator
      opByte = input.getByte()
      eoi = (opByte & (1 << 0)) > 0
      srq = (opByte & (1 << 3)) > 0
      hasData = (opByte & (1 << 7)) > 0
      version = input.getByte()
      seqno = input.getByte()
      spare = input.getByte() // spare, ignored
      dataLen = input.getInt()
      data <- IO take dataLen
    } yield {
      if (version != 1) throw new RuntimeException("Cannot handle unknown VICP version " + version)
      if (!hasData && !data.isEmpty) throw new RuntimeException("Strange VICP package with false data flag but containing data")
      VICPMsg(seqno = seqno, srq = srq, eoi = eoi, data = if (hasData) Some(data) else None)
    }
  }


  class VICPLinkImpl(val uri: String) extends
    VICPLink with ByteStreamIOImpl
  {
    val stream = InetConnection(uri, "inet-connection")
    val streamARef = actorRef(stream)
    log.debug("VICP over stream " + streamARef)
    context.watch(streamARef)

    stream.recv(selfRef, VICPCodec.dec, repeat = true)
    
    val dataBuilder = ByteStringBuilder()

    def flush(): Unit = {
      val bytes = outputQueue.result
      if (! bytes.isEmpty) {
        val msg = VICPMsg(eoi = true, data = Some(bytes))
        stream.send(msg, VICPCodec.enc)
        outputQueue.clear
      }
    }

    override def msgReceive = extend(super.msgReceive) {
      case (msg: VICPMsg, _) => {
        log.trace("Received VICP message " + loggable(msg))
        msg.data match {
          case Some(data) => {
            dataBuilder ++= data
            if (msg.eoi) {
              log.trace("Data complete")
              inputQueue pushData dataBuilder.result
              dataBuilder.clear()
            }
          }
          case None =>
        }
      }
    }
  }
}
