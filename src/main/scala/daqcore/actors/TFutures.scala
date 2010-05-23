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


// This FutureActor is a modified version of Philipp Haller's FutureActor 

private class TFutureActor[T](fun: SyncVar[Either[T, Throwable]] => Unit, channel: Channel[T]) extends Future[T] with DaemonActor {
  val res = new SyncVar[Either[T, Throwable]]

  var enableChannel = false // guarded by this

  def isSet = !fvalue.isEmpty

  def apply(): T = {
    if (fvalue.isEmpty) {
      this !? Eval
    }
    res.get match { case Left(v) => v; case Right(t) => throw t }
  }

  def respond(k: T => Unit) {
    if (isSet) k(fvalueTyped)
    else {
      val ft = this !! Eval
      ft.inputChannel.react {
        case _ => res.get match { case Left(v) => k(v); case Right(t) => throw t }
      }
    }
  }

  def inputChannel: InputChannel[T] = {
    synchronized {
      if (!enableChannel) {
        if (isSet)
          channel ! fvalueTyped
        enableChannel = true
      }
    }
    channel
  }

  def act() {
    trapExit = true
     val a = Actor.link { fun(res) }

    react { case Exit(`a`, reason) =>
      if (!res.isSet) reason match {
          case UncaughtException(_, _, _, _, t) => res.set(Right(t))
          case r => res.set(Right(new RuntimeException(r.toString)))
      }
      
      synchronized {
        val v = res.get
        fvalue =  Some(v)
        if (enableChannel)
          channel ! v.left.get
      }

      loop {
        react {
          case Eval => reply()
        }
      }
    }

  }

}


object TFutures {
  def tfuture[T](body: => T): Future[T] = {
    val c = new Channel[T](Actor.self(DaemonScheduler))
    val a = new TFutureActor[T](_.set(Left(body)), c)
    a.start()
    a
  }

  def tfuture[T](resp: Responder[T]): Future[T] = {
    val c = new Channel[T](Actor.self(DaemonScheduler))
    val a = new TFutureActor[T](fv => resp.respond{rv => fv.set(Left(rv))}, c)
    a.start()
    a
  }
}
