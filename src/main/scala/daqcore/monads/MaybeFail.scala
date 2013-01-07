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


abstract class MaybeFail[+A] extends Product {
  def get: A = this match {
    case Ok(value) => value
    case Fail(exception) => throw exception
  }

  def apply(): A = get
  
  def isOk: Boolean
  def isFail: Boolean = !isOk
  
  def toEither: Either[Throwable, A]

  def map[B](f: A => B): MaybeFail[B]

  def foreach(body: A => Unit) = { body(get) }
}


object MaybeFail {
  def apply[A](f: => A): MaybeFail[A] =
    try { Ok(f) }
    catch { case e: Throwable => Fail(e) }
}


case class Ok[+A](v: A) extends MaybeFail[A] {
  def isOk = true

  def toEither = Right(v)

  def map[B](f: A => B): MaybeFail[B] = Ok(f(v))
}


case class Fail(e: Throwable) extends MaybeFail[Nothing] {
  def isOk = false

  def toEither = Left(e)

  def map[B](f: Nothing => B): MaybeFail[B] = this
}
