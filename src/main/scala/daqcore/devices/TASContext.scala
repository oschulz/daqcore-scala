// Copyright (C) 2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import akka.actor.ActorRef

import daqcore.util._
import daqcore.actors._




trait TimeContext {
  def time: NanoTimeWindow
}

object TimeContext {
  def apply(time: NanoTime): TimeContext = TimeContextImpl(NanoTimeWindow(time))

  case class TimeContextImpl(time: NanoTimeWindow) extends TimeContext
}



// Timestamped Actor/State Context
trait TASContext extends TimeContext {
  def actor: ActorRef
  def state: StateCount

  // TODO: Add map, flatMap, etc.
}

case object TASContext {
  def apply(time: NanoTime, actor: ActorRef, state: StateCount): TASContext =
    TASContextImpl(NanoTimeWindow(time), actor, state)

  def apply(time: NanoTimeWindow, actor: ActorRef, state: StateCount): TASContext =
    TASContextImpl(time, actor, state)

  case class TASContextImpl(
    time: NanoTimeWindow,
    actor: ActorRef,
    state: StateCount
  ) extends TASContext
}
