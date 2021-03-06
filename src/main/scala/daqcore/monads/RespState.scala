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


package daqcore.monads


sealed trait RespState[S, +A] {
  def apply(s: S): Responder[(S, A)]

  import RespStates._

  def map[B](f: A => B): RespState[S, B] = state { prev =>
    apply(prev) map { _ match { case (next, a) => (next, f(a)) } }
  }

  def flatMap[B](f: A => RespState[S, B]): RespState[S, B] = state { prev =>
    apply(prev) flatMap { _ match { case (next, a) => f(a).apply(next) } }
  }
}


object RespStates {
  def state[S, A](f: S => Responder[(S, A)]): RespState[S, A] = new RespState[S, A] {
    def apply(s: S) = f(s)
  }

  def transform[S, A](f: S => (S, A)): RespState[S, A] =
    state { s => Responder.constant( f(s) ) }

  def init[S]: RespState[S, S] =
    transform { s => (s, s) }

  def modify[S](f: S => S): RespState[S, Unit] =
    transform { s => (f(s), ()) }
}
