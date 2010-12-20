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


package daqcore.actors

import scala.collection.mutable.{HashMap, SynchronizedMap}

import akka.actor._

import daqcore.util._


trait PStateServer extends Server {
  import PStateServer._

  type State

  protected var state: State 
  
  override def preRestart(reason: Throwable) {
    super.preRestart(reason)
    stateMap.put(self.uuid, state)
    log.debug("Stored restart-persistent state for actor %s: %s".format(self, loggable(state)))
  }

  override def preStart = {
    if (self.isBeingRestarted) {
      state = stateMap.get(self.uuid) match {
        case Some(state) => state.asInstanceOf[State]
        case None => throw new RuntimeException("Could not load restart-persistent state for actor %s".format(self))
      }
      log.debug("Loaded restart-persistent state for actor %s: %s".format(self, loggable(state)))
    }
    super.preStart
  }

  override def postStop {
    super.postStop
    log.debug("Removing restart-persistent state for actor " + self)
    stateMap.remove(self.uuid)
  }
}


object PStateServer {
  protected val stateMap = new HashMap[Uuid, Any] with SynchronizedMap[Uuid, Any]
}
