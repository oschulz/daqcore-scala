// Copyright (C) 2010-2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import java.net.InetAddress
import scala.annotation.tailrec
import java.io.IOException
import java.net.InetAddress
import java.util.concurrent.TimeoutException

import akka.actor._
import akka.dispatch.{Future, Promise}
import akka.util.{ByteString, Duration, Timeout}
import akka.util.duration._

import org.acplt.oncrpc.OncRpcProtocols
import daqcore.io.oncrpc.vxi11core

import daqcore.util._
import daqcore.actors._, daqcore.actors.TypedActorTraits._


trait VXI11Link extends RawMsgIO with CloseableTA {
}


object VXI11Link {
  def apply(address: InetAddress, device: String)(implicit rf: ActorRefFactory): VXI11Link =
    typedActorOf[VXI11Link](new LinkImpl(address, device))


  class LinkImpl(val address: InetAddress, val device: String) extends
    VXI11Link with ByteStreamIOImpl
  {
    val defaultRequestSize: Int = 0x400000 // 4 Mb
    val defaultMaxRecvSize:Int = 4096
    val defaultTimeout = 5000 // 5ms

    case class LinkHandle(id: Int) { def lid = new vxi11core.Device_Link(id) }
    
    val clnt: vxi11core.Client = {
      log.debug("Opening VXI11 client connection to " + address)
      try { new vxi11core.Client(address, OncRpcProtocols.ONCRPC_TCP) }
      catch { case e => throw new java.io.IOException("Could not open VXI11 client connection to " + address, e) }
    }
    atCleanup {
      clnt.close()
      log.debug("VXI11 client connection to " + address + "closed")
    }

    val lnk: LinkHandle = {
      log.debug("Opening VXI11 link to device " + device)
      try { openLinkImpl(device, defaultTimeout) }
      catch { case e => throw new java.io.IOException("Could not open VXI11 link to device " + device, e) }
    }
    atCleanup {
      closeLinkImpl(lnk)
      log.debug("VXI11 link to device " + device + " closed")
    }
    
    def flush(): Unit = {
      val bytes = outputQueue.result
      if (! bytes.isEmpty) {
        writeLinkImpl(lnk, defaultTimeout, bytes)
        outputQueue.clear
      }
    }
    
    protected def openLinkImpl(device: String, timeout: Long): LinkHandle = {
      log.debug("Creating new VXI11 link to %s, device %s".format(address, device))
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
      
      LinkHandle(lnkId)
    }


    protected def closeLinkImpl(lnk: LinkHandle): Unit = {
      log.trace("destroy_link(%s)".format(lnk.lid.value))
      val unlinkResp = clnt.destroy_link_1(lnk.lid)
      log.trace("destroy_link error value: " + unlinkResp.error.value)
    }
    

    override def recv[A](decoder: Decoder[A]): Future[A] = {
      val result = super.recv(decoder)
      while (inputQueue.pendingDecoders > 0)
        inputQueue.pushData(readLinkImpl(lnk, defaultTimeout))
      result
    }


    def readLinkImpl(lnk: LinkHandle, timeout: Long): ByteString = {
      val acc = new ByteStringBuilder
      
      @tailrec def readImpl(timeout: Long): ByteString = {
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
            
            acc ++= rresp.data
            // if end or chr bit set, read is complete, if not, more chunks to read
            if ((rresp.reason & (rcv_reason_end | rcv_reason_chr)) != 0) {
              log.trace("Finished reading")
              acc.result()
            } else {
              log.trace("Partial read")
              readImpl(timeout)
            }

          case 4|15|17 => throw new java.util.concurrent.TimeoutException("VXI11 read timed out")
          case 11 => throw new IOException("VXI11 read: Device locked by another link")
          case 23 => throw new IOException("VXI11 read: Abort")
          case _ => throw new IOException("VXI11 read: Unknown error")
        }
      }

      readImpl(timeout)
    }

    
    def writeLinkImpl(lnk: LinkHandle, timeout: Long, data: ByteString): Unit = {
      @tailrec def writeImpl(timeout: Long, data: ByteString, lastChunk: Boolean = true): Unit = {
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
          writeImpl(timeout, data.drop(bytesWritten))
      }

      writeImpl(timeout, data)
    }

  }

}
