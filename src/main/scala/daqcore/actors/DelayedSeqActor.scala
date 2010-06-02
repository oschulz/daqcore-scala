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

import scala.actors._, scala.actors.Actor._

import daqcore.util._


class DelayedSeqActor[T: ClassManifest] extends Actor {
  da =>
  
  protected object state {
    var error: Option[Throwable] = None
    var input: Option[T] = None
    def check = if (error != None) throw error.get
    
    def setError(err: Throwable): Unit = synchronized {
        error = Some(err)
        notifyAll
    }
    
    def push(value: T): Unit = synchronized {
      assert(input.isEmpty)
      input = Some(value)
      notifyAll
    }
    
    def pop(): T = synchronized {
      check
      if (input.isEmpty) wait()
      check
      val result = input.get
      input = None
      result
    }
  }
  
  def stream: LazyStream[T] = LazyStream.cons(state.pop(), stream)
  val seq: Seq[T] = stream
  var seqLast = seq

  object monitor extends Actor {
    def act() = {
      link(da)
      trapExit = true
      react {
        case Exit(`da`, reason) => state.synchronized {
          val error = reason match {
            case UncaughtException(_, _, _, _, e) => new RuntimeException(da.toString + " shut down, reason: " + e.toString)
            case r => new RuntimeException(da.toString + " shut down, reason: " + r.toString)
          }
          state.setError(error)
        }
        case x => daqcore.actors.kill(da, "Internal error, monitor received unexpected message")
      }
    }
  }
  monitor.start()
  
  val mfT = classManifest[T]
  
  def act() = {
    loop { react { case a => {
      val mfA = classMF(a)
      if (! (mfA <:< mfT)) throw new java.lang.ClassCastException("%s cannot be cast to %s".format(mfA, mfT))
      state.push( a.asInstanceOf[T] )
      seqLast = seqLast.tail
    } } }
  }
}


object DelayedSeqActor {
  def apply[T: ClassManifest](): DelayedSeqActor[T] = {
    val da = new DelayedSeqActor[T]
    da.start
    da
  }
}
