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


package scala.actors
package contrib.daqcore

import scala.actors.scheduler.DaemonScheduler
import scala.concurrent.SyncVar
import scala.actors._


object Futures {
  def unlinkedF[T](body: => T): Future[T] = {
    val c = new Channel[T](Actor.self(DaemonScheduler))
    val a = new FutureActor[T](_.set(body), c)
    a.start()
    a
  }

  def unlinkedF[T](resp: Responder[T]): Future[T] = {
    val c = new Channel[T](Actor.self(DaemonScheduler))
    val a = new FutureActor[T](fv => resp.respond{rv => fv.set(rv)}, c)
    a.start()
    a
  }

  def linkedF[T](body: => T): Future[T] = {
    val c = new Channel[T](Actor.self(DaemonScheduler))
    val a = new FutureActor[T](_.set(body), c)
  Actor.link(a)
    a.start()
    a
  }

  def linkedF[T](resp: Responder[T]): Future[T] = {
    val c = new Channel[T](Actor.self(DaemonScheduler))
    val a = new FutureActor[T](fv => resp.respond{rv => fv.set(rv)}, c)
  Actor.link(a)
    a.start()
    a
  }
}
