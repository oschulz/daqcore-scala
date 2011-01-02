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

import collection.IndexedSeqLike
import collection.mutable.{Builder,ArrayBuffer,ArrayBuilder}


class ByteCharSeq(protected val buffer: Array[Byte], val from: Int, val until: Int) extends CharSequence
  with scala.collection.immutable.IndexedSeq[Byte]
  with IndexedSeqLike[Byte, ByteCharSeq]
{
  override def iterator = ArrayIterator.forArrayRange(buffer, from, until)

  require((from <= until) && (until <= buffer.length))

  def length = until - from

  override final def apply(index: Int): Byte = {
    if (index < length) buffer(from + index)
    else throw new java.lang.IndexOutOfBoundsException(index.toString)
  }
  
  override def copyToArray[B >: Byte](xs: Array[B], start: Int, len: Int): Unit =
    iterator.copyToArray(xs, start, len)

  override def toArray[B >: Byte](implicit arg0: ClassManifest[B]): Array[B] =
    iterator.toArray
  
  override def foreach[U](f: (Byte) => U): Unit =
    for (i <- this.from to this.until-1) f(buffer(i))

  def charAt(index: Int) = apply(index).toChar

  def subSequence(from: Int = 0, until: Int = length) =
    new ByteCharSeq(buffer, this.from + from, this.from + until)
  
  def sharedWith(that: ByteCharSeq): Boolean = this.buffer eq that.buffer
  
  def ++(that: ByteCharSeq): ByteCharSeq = {
    if ((this sharedWith that) && (this.until == that.from)) {
      new ByteCharSeq(this.buffer, this.from, that.until)
    } else {
      val a = Array.ofDim[Byte](this.length + that.length)
      this.copyToArray(a, 0, this.length)
      that.copyToArray(a, this.length, that.length)
      new ByteCharSeq(a, 0, a.length)
    }
  }
  
  def ++(that: Seq[Byte]): ByteCharSeq = this ++ ByteCharSeq(that: _*)
  def ++(s: String): ByteCharSeq = this ++ ByteCharSeq(s)
  
  override def toString =
    new String(toArray, ByteCharSeq.encoding)
  
  override protected def newBuilder: Builder[Byte, ByteCharSeq] = ByteCharSeq.newBuilder
}


object ByteCharSeq {
  def encoding  = "ASCII"

  def apply(): ByteCharSeq = empty
  
  def apply(bytes: Byte*): ByteCharSeq = bytes match {
    case seq: ByteCharSeq => seq
    case seq => apply(bytes.toArray)
  }
  
  def apply(seq: CharSequence): ByteCharSeq = seq match {
    case seq: ByteCharSeq => seq
    case seq => apply(seq.toString)
  }
  
  def apply(array: Array[Byte]): ByteCharSeq = new ByteCharSeq(array, 0, array.length)
  
  def apply(s: String): ByteCharSeq = apply(s.getBytes(encoding))
  
  def apply(char: Char): ByteCharSeq = apply(char.toString)

  val empty = apply(Array.empty[Byte])

  def newBuilder: Builder[Byte, ByteCharSeq] =
    new ArrayBuilder.ofByte() mapResult { a => apply(a.toArray) }
  
  val lf = ByteCharSeq('\n')
  val cr = ByteCharSeq('\r')
  val crlf = ByteCharSeq("\r\n")
}
