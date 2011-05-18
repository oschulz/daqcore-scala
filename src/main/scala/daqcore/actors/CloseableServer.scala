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

import scala.collection.immutable.Queue

import akka.actor.ActorRef


trait CloseableServer extends CascadableServer {
  override def profiles = super.profiles.+[Closeable]
  
  var closeListeners: Queue[ActorRef] = Queue[ActorRef]()

  override def init() = {
    super.init()
    
    atCleanup {
      while (! closeListeners.isEmpty) {
        val (listener, rest) = closeListeners.dequeue
        closeListeners = rest
        try { listener ! Closeable.Closed } catch { case _ => }
      }
    }
  }

  /*def srvClose(): Unit = {
    log.debug("Closing.")
    self.stop()
  }*/
  
  def srvNotifyOnClose(receiver: ActorRef): Unit = {
    closeListeners = closeListeners enqueue receiver
  }

  override def serve = super.serve orElse {
    // case Closeable.Close => { srvClose() }
    case Closeable.NotifyOnClose(receiver) => { srvNotifyOnClose(receiver) }
  }
}
