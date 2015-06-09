// Copyright (C) 2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.util

import scala.language.{implicitConversions, higherKinds}
import scala.concurrent.{Future, ExecutionContext}
import scala.collection.generic.CanBuildFrom
import scala.collection.breakOut


package object concurrent {
  implicit def combineFutureSeq1[T, M[X] <: Traversable[X]]
    (xs: M[Future[T]]) (implicit
      cbf: CanBuildFrom[M[Future[T]], T, M[T]],
      executor: ExecutionContext
    ): Future[M[T]] = Future.sequence(xs)

  implicit def combineFutureSeq2[T](xs: Seq[Seq[Future[T]]])(implicit executor: ExecutionContext): Future[Seq[Seq[T]]] =
    combineFutureSeq1(xs map { x => combineFutureSeq1(x) })

  implicit def combineFutureSeq3[T](xs: Seq[Seq[Seq[Future[T]]]])(implicit executor: ExecutionContext): Future[Seq[Seq[Seq[T]]]] =
    combineFutureSeq1(xs map combineFutureSeq2)


  implicit def combineUnitFutureSeq1(xs: Traversable[Future[Unit]])(implicit executor: ExecutionContext): Future[Unit] =
    Future.sequence(xs) map { _ => {} }

  implicit def combineUnitFutureSeq2(xs: Traversable[Traversable[Future[Unit]]])(implicit executor: ExecutionContext): Future[Unit] =
    combineUnitFutureSeq1(xs map combineUnitFutureSeq1)

  implicit def combineUnitFutureSeq3(xs: Traversable[Traversable[Traversable[Future[Unit]]]])(implicit executor: ExecutionContext): Future[Unit] =
    combineUnitFutureSeq1(xs map combineUnitFutureSeq2)


  def ftconv[T](future: Future[T]): Future[T] = future
}
