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

import scala.util.control.ControlThrowable
import java.util.concurrent.TimeoutException


case object DelayedTimeout extends TimeoutException("Delayed value timed out.")


abstract trait Delayed[+A] {
  def apply(): A = get
  val get: A

  def avail: Boolean
  
  def map[B](f: A => B): Delayed[B] =
    new DelayedView[A, B](this) { lazy val get = f(orig.get) }

  def flatMap[B](f: A => Delayed[B]): Delayed[B] =
    new DelayedView[A, B](this) { lazy val get = f(orig.get).get }
  
  def foreach[U](body: A => Unit): Unit = { body(this.get) }
}


abstract class DelayedView[+A, +B](protected val orig: Delayed[A]) extends Delayed[B] {
  def avail = orig.avail
}


class DelayedVal[A](timeout: Option[Long]) extends Delayed[A] {
  def this() = this(None)
  def this(timeout: Int) = this(Some(timeout.toLong))

  protected def milliTime = java.lang.System.nanoTime / 1000000
  protected def age = milliTime - tStart
  protected val tStart = milliTime

  protected var optValue: Option[MaybeFail[A]] = None

  lazy val get: A = synchronized {
    optValue match {
      case Some(value) => value.get
      case None =>
        timeout match {
          case None => wait
          case Some(tmax) => {
            val remainWait = tmax - age;
            if (remainWait > 0) wait(remainWait)
            if (optValue == None) {
              optValue = Some(Fail(DelayedTimeout))
              notifyAll()
            }
          }
        }
        optValue.get.get
    }
  }
  
  def avail: Boolean = synchronized { optValue != None }

  def set(value: MaybeFail[A]): Unit = synchronized {
    optValue match {
      case None => optValue = Some(value); notifyAll()
      case Some(Fail(DelayedTimeout)) => throw DelayedTimeout
      case _ => new java.lang.RuntimeException("Delayed value already set.")
    }
  }
  
  def !(msg: Any): Unit = {
    try { set(Ok(msg.asInstanceOf[A])) }
    catch { case e => set(Fail(e)) } 
  }
}


object Delayed {
  def run[A](resp: Responder[A]): Delayed[A] =
    buildFrom[A](None) { res => resp.respond { v => res.set(Ok(v)) } }

  def forked[A](timeout: Option[Long])(v: =>A): Delayed[A] =
    buildFromForked[A] (timeout) { res => res.set(Ok(v)) }

  
  def buildFrom[A](timeout: Option[Long])(body: DelayedVal[A] => Unit): Delayed[A] = {
    val res = new DelayedVal[A]
    actors.Actor.actor {
      try { body(res) }
      catch { case e => if (!res.avail) res.set(Fail(e)) }
    }
    res
  }

  def buildFromForked[A](timeout: Option[Long])(body: DelayedVal[A] => Unit): Delayed[A] = {
    import scala.actors._
    import scala.actors.Actor._
    
    val res = new DelayedVal[A](timeout)

    actor {
      val supervisor = self
      self.trapExit = true
      val worker = actor {
        link(supervisor)
        body(res)
      }
      react { case Exit(`worker`, reason) =>
        if (!res.avail) reason match {
          case UncaughtException(_, _, _, _, e) => res.set(Fail(e))
          case r => res.set(Fail(new RuntimeException(r.toString)))
        }
      }
    }
    
    res
  }
}
