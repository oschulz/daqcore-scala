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

import java.io.{File, InputStream, FileInputStream}
import akka.actor.Actor.actorOf

import daqcore.util._


class DoEvery(action: => Unit, interval: Int, after: Int) extends CascadableServer {
  override def profiles = super.profiles.+[Repeated]

  val maxChunkSize = 512 * 1024
  
  var paused = false
  var count = 0
  
  override def init(): Unit = {
    super.init()
    sendAfter(after, srv, RunAction)
  }
  
  protected case object RunAction
  
  override def serve = super.serve orElse {
    case Repeated.Pause =>
      paused = true

    case Repeated.IsRunning => reply(! paused)
    
    case Repeated.GetCount => reply(count)

    case Repeated.Resume =>
      if (paused) { paused = false; srv ! RunAction }

    case RunAction => if (!paused) {
      sendAfter(interval, srv, RunAction)
      action
      count = count + 1
    }
  }
}


object DoEvery {
  def apply(interval: Int, after: Int = 0, sv: Supervising = defaultSupervisor)(action: => Unit): Repeated =
    new ServerProxy(sv.linkStart(actorOf(new DoEvery(action, interval, after)))) with Repeated
}
