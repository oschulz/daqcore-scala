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

import akka.dispatch.{Future, FutureTimeoutException}

import daqcore.monads._


trait Ft[A] {
  ft =>

  def get: Option[A]

  def apply(): A

  def foreach(k: A => Unit): Unit = k(apply())
  
  def map[B](f: A => B): Ft[B]

  /*def flatMap[B](f: A => Ft[B]): Ft[B] = new Ft[B] {
    def get = ft.get flatMap { f(_).get }
  }*/
  
  override def toString = "Ft[]"
}


class AkkaFt[A](future: Future[A]) extends Ft[A] {
  ft =>

  def get: Option[A] = {
    try { future.await } 
    catch { case e: FutureTimeoutException => None }
    if (future.exception.isDefined) throw future.exception.get
    else future.result
  }

  def apply(): A = {
    try { future.await } 
    catch { case e: FutureTimeoutException => throw e }
    if (future.exception.isDefined) throw future.exception.get
    else future.result.get
  }
  
  def map[B](f: A => B): Ft[B] = new AkkaFt(future.map(f))

  override def toString = "AkkaFt(%s)".format(future.toString)
}
