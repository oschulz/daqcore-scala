// Copyright (C) 2013 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.devices

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import akka.actor._

import daqcore.actors._
import daqcore.util._
import daqcore.io._


trait DeviceKeepAlive extends Device with TypedActorBasics with TypedActorReceive {
  import daqcore.defaults.defaultTimeout

  protected def getIdentity(): String

  protected val keepAliveInterval = 10.seconds

  protected lazy val idn = getIdentity()

  def identity = successful(idn)
  
  scheduleSelf(0.seconds, 10.seconds) {
    log.trace("Connection checked")
    assert( getIdentity() == idn )
  }
}
