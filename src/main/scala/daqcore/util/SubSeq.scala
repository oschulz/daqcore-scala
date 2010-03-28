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

class SubIdxSeq[+A](data: IndexedSeq[A], start: Int, end: Int)
  extends scala.collection.immutable.IndexedSeq[A]
  with GenericTraversableTemplate[A, SubIdxSeq]
  with IndexedSeqLike[A, SubIdxSeq[A]]
{
  if (start < 0) throw new IndexOutOfBoundsException(start.toString)
  if ((end < start) || (data.length < end)) throw new IndexOutOfBoundsException(end.toString)

  def length = end - start
  
  protected def buffer = data
  def sharedWith[B >: A](that: SubIdxSeq[B]): Boolean = buffer eq that.buffer
  
  def apply(index: Int) = {
    val i = index + start
    if (index >= end) throw new IndexOutOfBoundsException(index.toString)
    data(i)
  }
  
  def subSequence(start: Int, end: Int) =
    new SubIdxSeq[A](data, this.start+start, this.start+end)

  override def toString = "SubIdxSeq(" + data.view(start, end).mkString(", ") + ")"
  
  override def companion: GenericCompanion[SubIdxSeq] = SubIdxSeq
}

object SubIdxSeq extends SeqFactory[SubIdxSeq] {
  def apply[A](seq: IndexedSeq[A]) : SubIdxSeq[A] = seq match {
    case subSeq: SubIdxSeq[_] => subSeq
    case seq: IndexedSeq[_] => new SubIdxSeq(seq, 0, seq.length)
  }

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, SubIdxSeq[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, SubIdxSeq[A]] = new ArrayBuffer[A] mapResult {buf => val seq = buf.toIndexedSeq; new SubIdxSeq(seq, 0, seq.length)}
}



class ByteCSeq(val contents: IndexedSeq[Byte]) extends CharSequence {
  def charAt(index: Int) = contents(index).toChar

  def length = contents.length
  
  def subSequence(start: Int, end: Int) =
    new ByteCSeq(SubIdxSeq(contents).subSequence(start, end))
  
  override def toString = contents.view map {_.toChar} mkString
}


object ByteCSeq {
  def encoding  = "ASCII"
  def apply(bytes: IndexedSeq[Byte]): ByteCSeq = new ByteCSeq(SubIdxSeq(bytes))
  def apply(s: String): ByteCSeq = apply(s.getBytes(encoding))
  def apply(seq: SubIdxSeq[Byte]) = new ByteCSeq(seq)
}
