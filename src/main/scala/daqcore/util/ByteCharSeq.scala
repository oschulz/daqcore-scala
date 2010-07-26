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


class ByteCharSeq(val contents: IndexedSeq[Byte]) extends CharSequence
  with scala.collection.immutable.IndexedSeq[Byte]
  with IndexedSeqLike[Byte, ByteCharSeq]
{
  def apply(index: Int): Byte = contents(index)

  def charAt(index: Int) = contents(index).toChar

  def length = contents.length
  
  def subSequence(start: Int = 0, end: Int = size) =
    new ByteCharSeq(contents.subSequence(start, end))
  
  def ++(that: ByteCharSeq): ByteCharSeq = new ByteCharSeq(this.contents ++ that.contents)
  def ++(that: Seq[Byte]): ByteCharSeq = new ByteCharSeq(this.contents ++ that)
  def ++(s: String): ByteCharSeq = this ++ ByteCharSeq(s)
  
  override def toString = contents.view map {_.toChar} mkString
  
  override protected def newBuilder: Builder[Byte, ByteCharSeq] = ByteCharSeq.newBuilder
}


object ByteCharSeq {
  def encoding  = "ASCII"

  def apply(): ByteCharSeq = empty
  def apply(bytes: Byte*): ByteCharSeq = bytes match {
    case seq: ByteCharSeq => seq
    case seq: IndexedSeq[_] => new ByteCharSeq(bytes.asInstanceOf[IndexedSeq[Byte]])
    case seq: Seq[_] => apply(bytes.toArray.toSeq.asInstanceOf[IndexedSeq[Byte]]: _*)
  }
  def apply(seq: CharSequence): ByteCharSeq = seq match {
    case seq: ByteCharSeq => seq
    case seq => apply(seq.toString)
  }
  def apply(array: Array[Byte]): ByteCharSeq = new ByteCharSeq(array.toSeq.asInstanceOf[IndexedSeq[Byte]])
  def apply(s: String): ByteCharSeq = apply(s.getBytes(encoding))
  def apply(char: Char): ByteCharSeq = apply(char.toString)

  val empty = apply(Array.empty[Byte])

  def newBuilder: Builder[Byte, ByteCharSeq] =
    new ArrayBuilder.ofByte() mapResult { a => apply(a.toSeq.asInstanceOf[IndexedSeq[Byte]]: _*) }
  
  val lf = ByteCharSeq('\n')
  val cr = ByteCharSeq('\r')
  val crlf = ByteCharSeq("\r\n")
}
