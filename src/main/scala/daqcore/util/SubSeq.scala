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


class SubIdxSeq[+T](val data: IndexedSeq[T], val start: Int, val end: Int) extends IndexedSeq[T] {
  if (start < 0) throw new IndexOutOfBoundsException(start.toString)
  if ((end < start) || (data.length < end)) throw new IndexOutOfBoundsException(end.toString)

  def length = end - start
  
  def apply(index: Int) = {
    val i = index + start
    if (index >= end) throw new IndexOutOfBoundsException(index.toString)
    data(i)
  }
  
  def subSequence(start: Int, end: Int) =
    new SubIdxSeq[T](data, this.start+start, this.start+end)

  override def toString = "SubIdxSeq(" + data.view(start, end).mkString(", ") + ")"
}

object SubIdxSeq {
  def apply[T](seq: IndexedSeq[T]) : SubIdxSeq[T] = seq match {
    case subSeq: SubIdxSeq[_] => subSeq
    case seq: IndexedSeq[_] => new SubIdxSeq(seq, 0, seq.length)
  }

  def apply[T](array: Array[T]) : SubIdxSeq[T] =
    array.toSeq.asInstanceOf[IndexedSeq[T]]
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
