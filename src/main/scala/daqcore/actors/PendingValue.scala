// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import akka.dispatch.{Future, Promise, ExecutionContext}


// Intended to be used within an actor, not thread safe.

case class PendingValue[A](initial: Option[A] = None)(implicit executor: ExecutionContext) {
  protected var valueVar: Option[A] = initial
  protected var promiseVar = newPromise(valueVar)

  protected def newPromise(optValue: Option[A]) = optValue match {
    case Some(x) => Promise successful x
    case None => Promise[A]()
  }

  def value = valueVar

  def value_=(optValue: Option[A]): Unit = if (valueVar != optValue) {
    valueVar = optValue
    optValue match {
      case Some(x) if (! promiseVar.isCompleted) => promiseVar success x
      case v => promiseVar = newPromise(v)
    }
  }

  def promise: Promise[A] = promiseVar
}
