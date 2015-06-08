// Copyright (C) 2011-2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore

import scala.language.implicitConversions

import scala.collection.breakOut
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.generic.CanBuildFrom
import scala.concurrent.{Future, ExecutionContext}


package object devices {
  type ChV[A] = SortedMap[Int, A]

  object ChV {
    def apply[A](chValues: (Int, A)*): ChV[A] = SortedMap(chValues: _*)

    def apply[A](channels: Traversable[Int], value: A): ChV[A] =
      channels.map{channel => (channel, value)}(breakOut)

    def apply[A, B](xs: Seq[A])(f: PartialFunction[(Int, A), (Int, B)]): ChV[B] =
      SortedMap( xs.zipWithIndex map {_.swap} collect f :_*)

    def newBuilder[A] = SortedMap.newBuilder[Int, A]

    def future[A](ft: Future[Traversable[(Int, A)]])(implicit executor: ExecutionContext): Future[ChV[A]] =
      ft map { xs => xs.map(identity)(breakOut) }
  }


  implicit class ChVOps[A](val chV: ChV[A]) extends AnyVal {
    def vMap[B](f: A => B): ChV[B] = chV map { case (channel, value) => (channel, f(value)) }

    def cvvMap[B](f: (Int, A) => B): ChV[B] = chV map { case (channel, value) => (channel, f(channel, value)) }

    def ftVMap[B](f: A => Future[B])(implicit executor: ExecutionContext): Future[ChV[B]] = {
      val itFt = chV.iterator.map{case (channel, oldValue) => f(oldValue) map {newValue => (channel, newValue)}}
      Future.sequence(itFt) map { ChV.newBuilder[B].++=(_).result }
    }

    def ftCVVMap[B](f: (Int, A) => Future[B])(implicit executor: ExecutionContext): Future[ChV[B]] = {
      val itFt = chV.iterator.map{case (channel, oldValue) => f(channel, oldValue) map {newValue => (channel, newValue)}}
      Future.sequence(itFt) map { ChV.newBuilder[B].++=(_).result }
    }
  }


  type Ch = SortedSet[Int]

  object Ch {
    def apply(ch: Int): Ch = SortedSet[Int](ch)
    def apply(ch: Int, channels: Int*): Ch = apply(channels) + ch
    def apply(channels: Traversable[Int]*): Ch = channels.flatMap(identity){breakOut}
  }



  implicit class IntChOps(val channel: Int) extends AnyVal {
    def -->[A](value: A): ChV[A] = ChV((channel, value))

    def vMap[A](f: Int => A): ChV[A] = ChV((channel, f(channel)))

    def ftVMap[A](f: Int => Future[A])(implicit executor: ExecutionContext): Future[ChV[A]] =
      f(channel) map { value => ChV((channel, value)) }
  }



  implicit class TraversableChOps(val channels: Traversable[Int]) extends AnyVal {
    def -->[A](value: A): ChV[A] = ChV(channels, value)

    def vMap[A](f: Int => A): ChV[A] = channels.map{channel => (channel, f(channel))}(breakOut)

    def ftVMap[A](f: Int => Future[A])(implicit executor: ExecutionContext): Future[ChV[A]] = {
      val itFt = channels.iterator.map{channel => f(channel) map {value => (channel, value)}}
      Future.sequence(itFt) map { ChV.newBuilder[A].++=(_).result }
    }
  }


  // Currently not possible, as it breaks the implicit Int.to (for some
  // reason, there is also a SortedSet.to):
  //   implicit def intToCh(channel: Int): Ch = Ch(channel)

  implicit def traversableToCh(xs: Traversable[Int]): Ch = Ch(xs)
}
