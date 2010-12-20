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

import java.io.{File}
import akka.actor._, akka.actor.Actor.actorOf

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._


class EventBroadcaster(val source: EventInput) extends CloseableServer with EventServer {
  override def profiles = super.profiles.+[EventSource]

  val sourceCompanion = EventInput

  def needMoreInput: Boolean = false

  override def init() = {
    super.init()

    clientLinkTo(source.srv)
    atShutdown{ source.srv.stop() }
    source.triggerRecv()
  }
  
  override def onServerExit(server: ActorRef, reason: Option[Throwable]) = {
    if ((server == source.srv) && (reason == None)) self.stop()
    else super.onServerExit(server, reason)
  }
  
  override def serve = super.serve orElse {
    case sourceCompanion.Received(data) => {
      source.triggerRecv()
      srvEmit(data)
    }
  }
}


object EventBroadcaster {
  def apply(source: EventInput, sv: Supervising = defaultSupervisor): EventSource = {
    new ServerProxy(sv.linkStart(actorOf(new EventBroadcaster(source)))) with EventSource
  }
}
