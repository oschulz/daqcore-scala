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


class SCPIMsgClient(msgLnk: MsgIO) extends Server with SCPIClientLink {
  var parser: SCPIParser = null

  protected case class ReadTo(target: OutputChannel[Any], timeout:Long)
  
  class ReadQueue extends DaemonActor with Logging {
    def act() = {
      link(msgLnk.srv)
      loop { react {
        case ReadTo(repl, timeout) => {
          for (optBytes <- msgLnk.readF(timeout)) {
            optBytes match {
              case Some(bytes) => {
                trace("Received bytes: %s".format(optBytes))
                
                val response = parser.parseResponse(bytes)
                trace("Received: %s".format(response.toString))
                repl ! response
              }
              case None => {
                trace("Read timed out")
                repl ! Timeout
              }
            }
          }
        }
        case _ => exit('unknownMessage)
      } }
    }
  }
  val readQueue = new ReadQueue

  override def init() = {
    parser = new SCPIParser
    link(msgLnk.srv)
    msgLnk.clearInput(100)
    link(readQueue)
    readQueue.startOrRestart()
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
      readQueue ! ReadTo(repl, cmdqry.timeout)
    }
    case Closeable.Close => {
      msgLnk.close()
      exit('closed)
    }
  }
}


object SCPIMsgClient {
  def apply (msgLnk: MsgIO): SCPIMsgClient =
    start(new SCPIMsgClient(msgLnk))
  
  def apply (streamLnk: StreamIO): SCPIMsgClient =
    SCPIMsgClient(GPIBOverStream(streamLnk))
}
