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


package daqcore.util

import scala.reflect.{ClassTag, classTag}
import scala.collection.mutable.ArrayBuilder


class SeqOps[A: ClassTag](thisSeq: Seq[A]) {
  def toIISeq = thisSeq.toArray.toSeq.asInstanceOf[IndexedSeq[A]]
}


object IISeq {
  def apply[A: ClassTag](v: A*) = v.toIISeq
   def fill[T: ClassTag](n: Int)(elem: => T) = Array.fill(n)(elem).toIISeq
}


class NestedSeqOps[A: ClassTag](thisSeq: Seq[Seq[A]]) extends SeqOps[Seq[A]](thisSeq) {
  def flat: Seq[A] = thisSeq.view.flatMap{a=>a}
  def flatWithSep(sep: Seq[A]): Seq[A] = new FlattenedWithSep(thisSeq, sep)
}


class FlattenedWithSep[A: ClassTag](val thisSeq: Seq[Seq[A]], val sep: Seq[A]) extends Seq[A] {
  lazy val length: Int = (thisSeq.view map {_.length} sum) + (0 max ((thisSeq.length - 1) * sep.length))
  
  lazy val contents: IndexedSeq[A] = toArray(classTag[A]).toSeq.asInstanceOf[IndexedSeq[A]]
  
  override def toArray[B >: A](implicit arg0: ClassTag[B]): Array[B] = {
    val builder = ArrayBuilder.make[B]
    var first = true
    for (s <- thisSeq) {
      if (!first) builder ++= sep
      else first = false
      builder ++= s
    }
    builder.result
  }
  
  def apply(idx: Int): A = contents(idx)
  
  def iterator: Iterator[A] = new Iterator[A] {
    val seqIt = thisSeq.iterator
    var elemIt = seqIt.next.iterator
    
    private def proceed =
      if (!elemIt.hasNext && seqIt.hasNext)
        elemIt = sep.iterator ++ seqIt.next.iterator
    
    def hasNext = { proceed; elemIt.hasNext }
    def next = { proceed; elemIt.next }
  }
}
