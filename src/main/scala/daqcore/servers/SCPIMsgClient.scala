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

import scala.actors._

import java.io.IOException
import java.net.InetAddress
import java.util.concurrent.TimeoutException

import org.acplt.oncrpc.OncRpcProtocols

import daqcore.oncrpc.vxi11core
import daqcore.actors._
import daqcore.profiles._
import daqcore.util._

import daqcore.prot.scpi._


class SCPIMsgClient(msgLnk: RawMsgIO) extends Server with SCPIClientLink {
  var parser: SCPIParser = null

  protected case class ReadResponse()
  
  class SCPIResponseInput(msgLnk: RawMsgIO) extends Server {
    val replyQueue = collection.mutable.Queue[MsgTarget]()

    protected override def init() = {
      super.init()
      link(msgLnk.srv)
    }

    protected def serve = {
      case ReadResponse() => {
        val replyTo = replyTarget
        msgLnk.srv ! RawMsgInput.Recv()
        replyQueue.enqueue(replyTo)
      }
      case RawMsgInput.Received(bytes) => {
          trace("Received %s bytes: %s".format(bytes.size, loggable(bytes)))
          
          val response = parser.parseResponse(ByteCharSeq(bytes: _*))
          trace("Received response: %s".format(loggable(response.toString)))
          replyQueue.dequeue() ! response
      }
    }
  }
  val responseInput = new SCPIResponseInput(msgLnk)

  override def init() = {
    parser = new SCPIParser
    link(msgLnk.srv)
    msgLnk.clearInput(100)
    link(responseInput)
    responseInput.startOrRestart()
  }

  def serve = {
    case cmd: SCPIClientLink.CmdOnly => {
      val request = cmd.request
      msgLnk.write(request.charSeq) // Append CR-LF?
      trace("Sent: %s".format(request.toString))
    }
    case cmdqry: SCPIClientLink.CmdQuery => {
      val repl = sender
      val request = cmdqry.request
      msgLnk.write(request.charSeq) // Append CR-LF?
      trace("Sent: %s".format(request.toString))
      responseInput forward ReadResponse()
    }
    case Closeable.Close => {
      msgLnk.close()
      exit('closed)
    }
  }
}


object SCPIMsgClient {
  def apply (msgLnk: RawMsgIO): SCPIMsgClient =
    start(new SCPIMsgClient(msgLnk))
  
  def apply (streamLnk: ByteIO): SCPIMsgClient =
    SCPIMsgClient(GPIBOverStream(streamLnk))
}
