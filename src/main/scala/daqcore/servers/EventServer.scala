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

import scala.actors._, scala.actors.Actor._

import daqcore.actors._
import daqcore.util._
import daqcore.profiles._


class EventServer extends Server with EventSource {
  case class ListenSpec(select: PartialFunction[Any, Any], once: Boolean)

  @volatile protected var listeners = Map.empty[AbstractActor, ListenSpec]
  
  protected def nListeners: Int = listeners.size
  protected def hasListeners: Boolean = ! listeners.isEmpty
  
  protected def doEmit(event: Any): Unit = {
      trace("Emitting event %s".format(loggable(event)))
      for {
        (listener, ListenSpec(select, once)) <- listeners
        if select isDefinedAt event
      } {
        listener ! select(event)
        if (once) doUnlisten(listener)
      }
  }
  
  protected def doListen(listener: AbstractActor, select: PartialFunction[Any, Any], once: Boolean): Unit = {
    debug("Adding %s as as listener %s, with select = %s".format(listener, nListeners+1, select) + (if (once) ", listen once" else ""))
    Actor.link(listener)
    listeners = listeners + (listener -> ListenSpec(select, once))
  }
  
  protected def doUnlisten(listener: AbstractActor): Unit = {
    debug("Removing listener %s".format(listener))
    Actor.unlink(listener)
    listeners = listeners - listener
  }
  
  protected def onListenerExit(listener: AbstractActor): Unit = {
    debug("%s exited, removing it from listeners".format(listener))
    listeners = listeners - listener
  }
  
  override protected def init() = {
    super.init()
    trapExit = true
  }

  protected def serve = {
    case EventSource.Emit(event) =>
      doEmit(event)
    case EventSource.Listen(listener, select, once) =>
      doListen(listener, select, once)
    case EventSource.Unlisten(listener) =>
      doUnlisten(listener)
    case Exit(from, reason) => {
      if (listeners contains from) onListenerExit(from)
      else exit(reason)
    }
  }
}
