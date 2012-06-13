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


final case class ByteCharSeq private (wrapped: ByteString) extends
  CharSequence with HasByteRep
{
  @inline def apply(index: Int): Char = charAt(index)

  @inline def charAt(index: Int): Char = wrapped(index).toChar

  def length = wrapped.length

  def subSequence(start: Int = 0, end: Int = length) = {
    if ( (0 > start) || (start > end) || (end > length) )
      throw new IndexOutOfBoundsException("Index out of bounds in ByteCharSeq.subSequence")
    new ByteCharSeq(wrapped.slice(start, end))
  }

  //!! Improve this, use internal array of wrapped ByteString to reduce copying
  override def toString = new String(wrapped.toArray, ByteCharSeq.encoding)


  override def getBytes: ByteString = toByteString
  
  def putBytes(builder: ByteStringBuilder) = builder ++= toByteString

  
  def ++(that: ByteCharSeq): ByteCharSeq = new ByteCharSeq(this.wrapped ++ that.wrapped)
  
  def ++(that: Seq[Byte]): ByteCharSeq = this ++ ByteCharSeq(that: _*)
  def ++(s: String): ByteCharSeq = this ++ ByteCharSeq(s)

  
  def toByteString: ByteString = wrapped

  def toSeq: ByteString = toByteString
}


object ByteCharSeq {
  def encoding  = "ASCII"

  def apply(): ByteCharSeq = empty
  
  def apply(bytes: Byte*): ByteCharSeq = bytes match {
    case seq: ByteString => new ByteCharSeq(seq)
    case _ => apply(ByteString(bytes.toArray))
  }

  def apply(seq: CharSequence): ByteCharSeq = seq match {
    case seq: ByteCharSeq => seq
    case seq => apply(seq.toString)
  }
  
  def apply(s: String): ByteCharSeq = apply(ByteString(s, encoding): _*)
  
  def apply(char: Char): ByteCharSeq = apply(char.toString)

  val empty = new ByteCharSeq(ByteString())
  
  val lf = ByteCharSeq('\n')
  val cr = ByteCharSeq('\r')
  val crlf = ByteCharSeq("\r\n")
}
