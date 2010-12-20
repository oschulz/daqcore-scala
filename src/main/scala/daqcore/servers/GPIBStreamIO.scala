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

import akka.actor.Actor.actorOf
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._


class GPIBStreamIO(val stream: ByteStreamIO) extends GPIBStreamInput with GPIBStreamOutput {
  override def profiles = super.profiles.+[RawMsgIO]

  val source = stream
  val target = stream
}


object GPIBStreamIO {
  def apply(stream: ByteStreamIO, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): RawMsgIO = {
    new ServerProxy(sv.linkStart(actorOf(new GPIBStreamIO(stream)), lc)) with RawMsgIO
  }
  
  def overInetStream(addr: InetSockAddr, timeout: Long = 10000, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): RawMsgIO = {
    val stream = InetConnection(addr, timeout, sv, lc)
    GPIBStreamIO(stream, sv, lc)
  }
}
