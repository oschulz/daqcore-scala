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

import scala.actors.Future, scala.actors.Futures
import java.lang.System.currentTimeMillis

import daqcore.monads._


object FTTimeout extends java.util.concurrent.TimeoutException("Future timed out")


trait Ft[+A] {
  ft =>

  def get: Option[A]

  def apply(): A = get.getOrElse(throw FTTimeout)

  def foreach(k: A => Unit): Unit = k(apply())
  
  def map[B](f: A => B): Ft[B] = new Ft[B] {
    def get = ft.get map f
  }

  def flatMap[B](f: A => Ft[B]): Ft[B] = new Ft[B] {
    def get = ft.get flatMap { f(_).get }
  }
  
  override def toString = "Ft[]"
}


object Ft {
  protected class WrappedFuture[+A](val future: Future[A], val timeout: TimeoutSpec) extends Ft[A] {
    val creationTime = currentTimeMillis
    
    lazy val get: Option[A] = {
      timeout match {
        case NoTimeout => Some(future.apply())
        case SomeTimeout(ms) => {
          val waitTime = ms - (currentTimeMillis - creationTime)
          if ((!future.isSet) && (waitTime > 0)) Futures.awaitAll(waitTime, future)
          if (future.isSet) Some(future.apply())
          else None
        }
      }
    }
  }

  def future[A](body: => A)(implicit timeout: TimeoutSpec) = new WrappedFuture(Futures.future(body), timeout)
  def apply[A](future: Future[A])(implicit timeout: TimeoutSpec) = new WrappedFuture(future, timeout)
}
