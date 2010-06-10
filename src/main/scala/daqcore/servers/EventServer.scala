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
import daqcore.profiles._


class EventServer extends Server with EventSource {
  @volatile protected var listeners = Map.empty[AbstractActor, PartialFunction[Any, Any]]
  
  override def init() = {
    super.init()
    trapExit = true
  }

  def serve = {
    case EventSource.Emit(event) => {
      trace("Emitting event %s".format(event))
      for {
        (listener, select) <- listeners
        if select isDefinedAt event
      } listener ! select(event)
    }
    case EventSource.Listen(listener, select) => {
      debug("Adding %s to listeners, with select = %s".format(listener, select))
      Actor.link(listener)
      listeners = listeners + (listener -> select)
    }
    case EventSource.Unlisten(listener) => {
      debug("Removing %s from listeners".format(listener))
      Actor.unlink(listener)
      listeners = listeners - listener
    }
    case Exit(from, reason) => {
      if (listeners contains from) {
        debug("%s exited, removing it from listeners".format(from))
        listeners = listeners - from
      }
      else exit(reason)
    }
  }
}