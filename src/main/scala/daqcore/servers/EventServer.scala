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

import akka.actor._, akka.actor.Actor.actorOf

import daqcore.actors._
import daqcore.util._
import daqcore.profiles._


trait EventServer extends CascadableServer {
  override def profiles = super.profiles.+[EventSource]

  @volatile var handlers = Set.empty[EventHandler]
  
  class ReplyingEventHandler[T](f: PartialFunction[Any, T], replyTo: MsgTarget) extends EventHandler {
    def handle = { case msg if f.isDefinedAt(msg) => replyTo ! f(msg); false }
  }
  
  def nHandlers: Int = handlers.size
  def hasHandlers: Boolean = ! handlers.isEmpty
  
  def srvEmit(event: Any): Unit = {
      trace("Emitting event %s".format(loggable(event)))
      for { handler <- handlers } {
        try if (handler.handle isDefinedAt event) {
          val again = handler.handle(event)
          if (!again) srvRemoveHandler(handler)
        }
        catch { case _ => srvRemoveHandler(handler) }
      }
  }
  
  def srvAddHandler(handler: EventHandler): Unit = {
    trace("Adding %s as as handler %s".format(handler, nHandlers+1))
    handlers = handlers + handler
  }
  
  def srvRemoveHandler(handler: EventHandler): Unit = {
    trace("Removing handler %s".format(handler))
    handlers = handlers - handler
  }
  
  override def serve = super.serve orElse {
    case EventSource.AddHandler(handler) =>
      srvAddHandler(handler)
    case EventSource.GetEvent(f) =>
      srvAddHandler(new ReplyingEventHandler(f, replyTarget))
    case EventSource.RemoveHandler(handler) =>
      srvRemoveHandler(handler)
  }
}



class EventSenderServer extends EventServer {
  override def profiles = super.profiles.+[EventSender]

  override def serve = super.serve orElse {
    case EventSender.Emit(event) =>
      srvEmit(event)
  }
}


object EventSenderServer {
  def apply(sv: Supervising = defaultSupervisor): EventSource =
    new ServerProxy(sv.linkStart(actorOf(new EventSenderServer()))) with EventSource
}
