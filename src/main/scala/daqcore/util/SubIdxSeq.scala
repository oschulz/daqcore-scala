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

import collection.generic._
import collection.mutable.{Builder,ArrayBuffer}
import collection.IndexedSeqLike


class SubIdxSeq[+A](parent: IndexedSeq[A], pstart: Int, pend: Int)
  extends scala.collection.immutable.IndexedSeq[A]
  with GenericTraversableTemplate[A, SubIdxSeq]
  with IndexedSeqLike[A, SubIdxSeq[A]]
{
  if (pstart < 0) throw new IndexOutOfBoundsException(pstart.toString)
  if ((pend < pstart) || (parent.length < pend)) throw new IndexOutOfBoundsException(pend.toString)

  protected val data: IndexedSeq[A] = parent match
    { case that: SubIdxSeq[_] => that.data; case that: IndexedSeq[_] => that }
  
  protected val start: Int = parent match
    { case that: SubIdxSeq[_] => that.start+pstart; case that: IndexedSeq[_] => pstart }

  protected val end: Int = parent match
    { case that: SubIdxSeq[_] => that.start+pend; case that: IndexedSeq[_] => pend }

  def length = end - start
  
  protected def buffer = data
  def sharedWith[B >: A](that: SubIdxSeq[B]): Boolean = buffer eq that.buffer
  
  def apply(index: Int) = {
    val i = index + start
    if ((i < start) || (index >= end)) throw new IndexOutOfBoundsException(index.toString)
    data(i)
  }
  
  def subSequence(start: Int = 0, end: Int = size) =  {
    if ((start == this.start) && (end == this.end)) this
    else new SubIdxSeq[A](this, start, end)
  }


  //override def filter(p: A => Boolean)
  //override def partition(p: A => Boolean) = (filter(p), filter(!p(_)))
  override def init = {
    if (isEmpty) throw new UnsupportedOperationException("empty.init")
    subSequence(0, size -1)
  }

  override def drop(n: Int) = subSequence(n max 0)
  override def take(n: Int) = subSequence(0, n)
  override def slice(from: Int, until: Int) = subSequence(from max 0, until)
  ///override def dropWhile(p: A => Boolean)
  ///override def takeWhile(p: A => Boolean)
  ///override def span(p: A => Boolean): (This, This) = (takeWhile(p), dropWhile(p))
  override def splitAt(n: Int) = (take(n), drop(n))


  override def ++[B >: A, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[SubIdxSeq[A], B, That]): That = that match {
    case that: SubIdxSeq[_] if (this.data == that.data) => {
      if (this.end == that.start) new SubIdxSeq[B](data, this.start, that.end).asInstanceOf[That]
      else if (that.end == this.start) new SubIdxSeq[B](data, that.start, this.end).asInstanceOf[That]
      else super.++[B, That](that)(bf)
    }
    case that => super.++[B, That](that)(bf)
  }

  override def toString = "SubIdxSeq(" + data.view(start, end).mkString(", ") + ")"
  
  override def companion: GenericCompanion[SubIdxSeq] = SubIdxSeq
}


object SubIdxSeq extends SeqFactory[SubIdxSeq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, SubIdxSeq[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, SubIdxSeq[A]] = new ArrayBuffer[A] mapResult {buf => val seq = buf.toSeq.asInstanceOf[IndexedSeq[A]]; new SubIdxSeq(seq, 0, seq.length)}
}
