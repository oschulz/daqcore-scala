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


package daqcore.io

import scala.actors._

import java.io.IOException
import java.net.InetAddress
import java.util.concurrent.TimeoutException

import akka.actor._, akka.actor.Actor._
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle, Temporary}

import org.acplt.oncrpc.OncRpcProtocols

import daqcore.io.oncrpc.vxi11core
import daqcore.actors._
import daqcore.util._

import daqcore.io.prot.scpi._


class SCPIMsgClient(msgLnk: RawMsgIO) extends Server with KeepAlive with PostInit with CloseableServer {
  override def profiles = super.profiles.+[SCPIClientLink]

  case class ReadResponse()
  
  class SCPIResponseInput(msgLnk: RawMsgIO) extends QueueingServer {
    val recvQueue = new ReplyQueue
    val parser: SCPIParser = new SCPIParser

    override def init() = {
      super.init()
      // msgLnk.clearInput(100) //!! Doesn't work here currently, will result in receiving "!(..., Received)" later
    }
  
    def serve = {
      case ReadResponse() => {
        msgLnk.triggerRecv()
        recvQueue.addTarget(replyTarget)
      }
      case RawMsgInput.Received(bytes) => {
          trace("Received %s bytes: %s".format(bytes.size, loggable(bytes)))
          
          val response = parser.parseResponse(bytes)
          trace("Received response: %s".format(loggable(response.toString)))
          recvQueue.addReply(response){}
      }
    }
  }

  var responseInput: ActorRef = _

  override def init() = {
    clientLinkTo(msgLnk.srv)
    atShutdown{ msgLnk.close() }
  }

  override def postInit() = {
    super.postInit()
    responseInput = srv.linkStart(actorOf(new SCPIResponseInput(msgLnk)), Temporary)
  }

  override def onServerExit(server: ActorRef, reason: Option[Throwable]) = {
    if ((msgLnk == msgLnk.srv) && (reason == None)) self.stop()
    else super.onServerExit(server, reason)
  }

  override def serve = super.serve orElse {
    case cmd: SCPIClientLink.CmdOnly => {
      val request = cmd.request
      msgLnk.send(request.getBytes) // Append CR-LF?
      trace("Sent: %s".format(request.toString))
    }
    case cmdqry: SCPIClientLink.CmdQuery => {
      val repl = replyTarget
      val request = cmdqry.request
      msgLnk.send(request.getBytes) // Append CR-LF?
      trace("Sent: %s".format(request.toString))
      responseInput forward ReadResponse()
    }
  }
}


object SCPIMsgClient {
  def apply(msgLnk: RawMsgIO, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): SCPIClientLink =
    new ServerProxy(sv.linkStart(actorOf(new SCPIMsgClient(msgLnk)), lc)) with SCPIClientLink
}
