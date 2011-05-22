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

import java.io.{File}

import akka.actor._, akka.actor.Actor._, akka.dispatch.Future

import daqcore.actors._
import daqcore.util._


trait InputFilterServer extends CascadableServer with QueueingServer {
  override def profiles = super.profiles.+[GenericInput]

  val inputCompanion: GenericInputCompanion
  import inputCompanion._

  def source: GenericInput
  val sourceCompanion: GenericInputCompanion

  val recvQueue = new ReplyQueue
  
  def needMoreInput: Boolean = false

  override def init() = {
    super.init()
    clientLinkTo(source.srv)
    atShutdown { source.close() }
    if (needMoreInput) source.triggerRecv()
  }
  
  override def onServerExit(server: ActorRef, reason: Option[Throwable]) = {
    if ((server == source.srv) && (reason == None)) self.stop()
    else super.onServerExit(server, reason)
  }

  def srvProcessInput(data: sourceCompanion.InputData): Seq[InputData]
  
  override def serve = super.serve orElse {
    case RawMsgInput.Recv() => {
      recvQueue addTarget replyTarget
      if ((recvQueue.pendingRequests > 0) && !needMoreInput)
        source.triggerRecv()
    }

    case sourceCompanion.Received(data) => {
      val results = srvProcessInput(data)
      for (msg <- results) {
        trace("Message available: [%s]".format(loggable(msg.toString)))
        recvQueue.addReply(Received(msg)){}
      }
      if ((recvQueue.pendingRequests > 0) || needMoreInput) source.triggerRecv()
    }
  }
}
