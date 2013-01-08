// Copyright (C) 2011-2013 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.collection.immutable.{SortedMap, SortedSet}


package object devices {
  type ChV[A] = SortedMap[Int, A]

  object ChV {
    def apply[A](values: (Int, A)*): ChV[A] = SortedMap(values: _*)

    def apply[A](v: (Seq[Int], A)): ChV[A] = apply((v._1 map {ch => (ch, v._2)}): _*)

    def apply[A, B](xs: Seq[A])(f: PartialFunction[(Int, A), (Int, B)]): ChV[B] =
      SortedMap( xs.zipWithIndex map {_.swap} collect f :_*)
  }


  type Ch = SortedSet[Int]

  object Ch {
    def apply(ch: Int): Ch = SortedSet[Int](ch)
    def apply(channels: Seq[Int]*): Ch = SortedSet[Int](channels.flatten: _*)
    def apply(ch: Int, channels: Int*): Ch = apply(channels) + ch
  }
}
