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

import java.io.{File, InputStream, FileInputStream}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._


class DoEvery(interval: Int, action: => Unit) extends Server with Repeated with Closeable {
  val maxChunkSize = 512 * 1024
  
  var paused = false
  var count = 0
  
  override protected def init(): Unit = {
    super.init()
    srv ! RunAction
  }
  
  protected case object RunAction
  
  override protected def serve = {
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

    case Closeable.Close => exit('closed)
  }
}


object DoEvery {
  def apply(interval: Int)(action: => Unit): Repeated =
    start(new DoEvery(interval, action))
}
