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

import scala.actors._

import daqcore.util.classMF


class AbstractActorOps(wrapped: AbstractActor) {
  def !?>[A](msg: Any) (f: PartialFunction[Any, A]) (implicit timeout: TimeoutSpec): A = {
    timeout match {
      case NoTimeout => f(wrapped.!?(msg))
      case SomeTimeout(ms) => f(wrapped.!?(ms,msg))
    }
  }

  def !!?(msg: Any)(implicit timeout: TimeoutSpec): Ft[Any] = Ft(wrapped !! msg)(timeout)
  
  def !!?>[A](msg: Any) (f: PartialFunction[Any, A]) (implicit timeout: TimeoutSpec): Ft[A] =
    wrapped.!!?(msg) map f
}


class ActorOps(wrapped: Actor) {
  def startOrRestart() = {
    try { wrapped.restart() }
    catch { case e: IllegalStateException => wrapped.start() }
  }
}
