// Copyright (C) 2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.servers

import scala.annotation.tailrec
import java.io.IOException
import java.net.InetAddress
import java.util.concurrent.TimeoutException

import akka.actor.Actor.actorOf, akka.actor._, akka.dispatch.Future
import akka.config.Supervision.{OneForOneStrategy, AllForOneStrategy, Temporary, Permanent}

import org.acplt.oncrpc.OncRpcProtocols

import daqcore.oncrpc.vxi11core
import daqcore.actors._
import daqcore.profiles._
import daqcore.util._


class RTVXI11Client(val address: InetAddress, timeout: Long) extends CloseableServer {
  val client = self

  override def profiles = super.profiles.+[VXI11Client]
  
  case class Crash(e: Throwable) extends ActorCmd
  
  class Link(val device: String, val timeout: Long, id: Int) extends CloseableServer {
    val lnk = SrvLid(self, id)
    
    override def profiles = super.profiles.+[VXI11ClientLink]
    
    override def init() = {
      super.init()
      self.lifeCycle = Permanent
      atCleanup{ try { client ! LinkClose(lnk) } catch { case e: ActorInitializationException => } }
    }
    
    override def serve = super.serve orElse {
      case RawMsgInput.Recv() => client forward LinkRead(lnk, timeout)
      case RawMsgOutput.Send(data) => client ! LinkWrite(lnk, timeout, data)
      case Crash(e) => throw(e)
    }
  }

  case class SrvLid(srv: ActorRef, id: Int) { def lid = new vxi11core.Device_Link(id) }

  val defaultRequestSize: Int = 0x400000 // 4 Mb
  val defaultMaxRecvSize:Int = 4096

  var clnt: vxi11core.Client = null  
  var rtlinks: Map[String, SrvLid] = null

  case class LinkRead(lnk: SrvLid , timeout: Long) extends ActorQuery[Seq[Byte]]
  case class LinkWrite(lnk: SrvLid, timeout: Long, data: Seq[Byte]) extends ActorCmd
  case class LinkClose(lnk: SrvLid) extends ActorCmd


  override def init() = {
    super.init()

    self.lifeCycle = Permanent
    self.faultHandler = OneForOneStrategy(List(classOf[Throwable]), 3, 1000)
    
    withCleanup {
      log.debug("Opening VXI11 client connection to " + address)
      try {
        clnt = new vxi11core.Client(address, OncRpcProtocols.ONCRPC_TCP)
      }
      catch { case e =>
        throw new java.io.IOException("Could not open VXI11 client connection to " + address, e)
      }
    } {
      clnt.close(); clnt = null;
      log.debug("VXI11 client connection to " + address + "closed")
    }

    withCleanup {
      rtlinks = Map.empty[String, SrvLid]
    } {
      for { (_, lnk) <- rtlinks } {
        try { srvCloseLink(lnk) } catch { case e => log.error(e.toString) }
        lnk.srv.stop
      }
      rtlinks = null
    }
  }


  override def serve = super.serve orElse {
    case LinkRead(lnk, timeout) => srvRead(lnk, timeout)
    case LinkWrite(lnk, timeout, data) => srvWrite(lnk, timeout, data)
    case LinkClose(lnk) => srvCloseLink(lnk)
    case VXI11Client.OpenLink(device, timeout) => srvOpenLink(device, timeout)
  }


  def crashOnError(srv: ActorRef)(body: => Unit) = {
    try { body }
    catch { case e => srv ! Crash(e) }
  }


  protected def srvOpenLink(device: String, timeout: Long): Unit = {
    log.debug("Creating new VXI11 link to %s, device %s".format(address, device))
    require(rtlinks.get(device) == None)
    require(timeout <= Int.MaxValue)
    
    val lparms = new vxi11core.Create_LinkParms
    lparms.lockDevice = false
    lparms.lock_timeout = timeout.toInt
    lparms.device = device

    log.trace("create_link(..., device = %s)".format(lparms.device))
    val lresp = clnt.create_link_1(lparms)

    log.trace("create_link error: " + lresp.error.value)

    // Notes from Steve D. Sharple's C/C++ VXI11 Library:
    // We need to check that maxRecvSize is a sane value (ie >0). Believe it
    // or not, on some versions of Agilent Infiniium scope firmware the scope
    // returned "0", which breaks Rule B.6.3 of the VXI-11 protocol. Nevertheless
    // we need to catch this, otherwise the program just hangs.
    val maxRecvSize = if (lresp.maxRecvSize > 0) lresp.maxRecvSize else defaultMaxRecvSize
    
    val lnkId = lresp.lid.value
    val lnkSrv = actorOf(new Link(device, timeout, lnkId))
    self.link(lnkSrv); lnkSrv.start
    rtlinks += device -> SrvLid(lnkSrv, lnkId)
    reply(lnkSrv)
  }


  protected def srvCloseLink(lnk: SrvLid): Unit = {
    rtlinks -= (rtlinks find {e => e._2.id == lnk.id} get)._1
    log.trace("destroy_link(%s)".format(lnk.lid.value))
    val unlinkResp = clnt.destroy_link_1(lnk.lid)
    log.trace("destroy_link error value: " + unlinkResp.error.value)
  }


  def srvRead(lnk: SrvLid, timeout: Long) : Unit = {
    @tailrec def readImpl(timeout: Long,
      acc: IndexedSeq[IndexedSeq[Byte]] = IndexedSeq.empty[IndexedSeq[Byte]]) :
      Option[ByteCharSeq] =
    {
      require(timeout <= Int.MaxValue)
      val rparms = new vxi11core.Device_ReadParms
      rparms.lid = lnk.lid
      rparms.io_timeout = timeout.toInt
      rparms.lock_timeout = timeout.toInt
      rparms.requestSize = defaultRequestSize
      rparms.flags = new vxi11core.Device_Flags(0)
      rparms.termChar = 0

      log.trace("device_read(%s, %s, ...)".format(rparms.lid, rparms.io_timeout))
      val rresp = clnt.device_read_1(rparms)
      // If read rpc call fails, there is nothing to read - retry or fail?
      // Catch/handle OncRpcException and/or IOException?

      log.trace("device_read error value: " + rresp.error.value)
      
      rresp.error.value match {
        case 0 => // OK
          val rcv_reason_end:Int = 0x04; // End indicator read
          val rcv_reason_chr:Int = 0x02; // Termchr set in flags and matching character transferred
          val rcv_reason_reqcnt:Int = 0x01; // requestSize bytes transferred.

          log.trace("device_read response reason: " + rresp.reason)
          
          if ((rresp.reason & rcv_reason_reqcnt) != 0)
            throw new IOException("VXI11 read: Request size to small")
          
          val boxedData: IndexedSeq[IndexedSeq[Byte]] = IndexedSeq(rresp.data)
          // if end or chr bit set, read is complete, if not, more chunks to read
          if ((rresp.reason & (rcv_reason_end | rcv_reason_chr)) != 0) {
            log.trace("Finished reading")
            Some(ByteCharSeq((acc ++ boxedData) flatten: _*))
          } else {
            log.trace("Partial read")
            readImpl(timeout, (acc ++ boxedData))
          }

        case 4|15|17 => // Timeout
          None
        case 11 => throw new IOException("VXI11 read: Device locked by another link")
        case 23 => throw new IOException("VXI11 read: Abort")
        case _ => throw new IOException("VXI11 read: Unknown error")
      }
    }

    crashOnError(lnk.srv) {
      readImpl(timeout) match {
        case Some(bytes: ByteCharSeq) => reply(RawMsgInput.Received(bytes))
        case None => // Timeout, no reply
      }
    }
  }

  
  def srvWrite(lnk: SrvLid, timeout: Long, data: Seq[Byte]): Unit = {
    @tailrec def writeImpl(timeout: Long, data: Seq[Byte], lastChunk: Boolean = true) :
      Unit =
    {
      require(timeout <= Int.MaxValue)
      val dataArray = data.toArray
      
      val wparms = new vxi11core.Device_WriteParms
      wparms.lid = lnk.lid
      wparms.io_timeout = timeout.toInt
      wparms.lock_timeout = timeout.toInt
      wparms.flags = new vxi11core.Device_Flags(if (lastChunk) 8 else 0);
      wparms.data = dataArray

      //!!! If data size > maxRecvSize, write seveal chunks!

      log.trace("device_write(%s, %s, ...)".format(wparms.lid, wparms.io_timeout))
      val wresp = clnt.device_write_1(wparms)
      // If write rpc call fails (!= RPC_SUCCESS) - retry or fail?
      // Catch/handle OncRpcException and/or IOException?
      //
      // Notes from Steve D. Sharple's C/C++ VXI11 Library:
      // The instrument did not acknowledge the write, just completely
      // dropped it. There was no vxi11 comms error as such, the 
      // instrument is just being rude. Usually occurs when the instrument
      // is busy.

      val bytesWritten = wresp.size
      log.trace("device_write error value: " + wresp.error.value)
      log.trace("bytes written: " + bytesWritten)
      if (bytesWritten < data.length)
        writeImpl(timeout, dataArray.drop(bytesWritten))
    }

    crashOnError(lnk.srv) { writeImpl(timeout, data) }
  }
}


object RTVXI11Client {
  def apply(address: InetAddress, timeout: Long = 10000, sv: Supervising = defaultSupervisor): VXI11Client =
      new ServerProxy(sv.linkStart(actorOf(new RTVXI11Client(address, timeout)))) with VXI11Client
}