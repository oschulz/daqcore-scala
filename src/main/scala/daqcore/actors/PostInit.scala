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

import akka.actor._


trait PostInit extends Server {
  import PostInit._

  protected[actors] var postInitDone = false
  
  def postInit(): Unit = {
    log.debug("postInit")
  }
  
  protected[actors] def runPostInitOnce() = {
    if (!postInitDone) {
      postInitDone = true
      postInit()
    }
  }

  override def receive = {
    case RunPostInit => {
      log.trace("received RunPostInit")
      runPostInitOnce()
    }
    case msg if (super.receive isDefinedAt msg) => {
      runPostInitOnce()
      super.receive(msg)
    }
  }

  override def preStart = {
    super.preStart
    self ! RunPostInit
  }
}


object PostInit {
  case object RunPostInit
}
