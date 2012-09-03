// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.io.prot.canopen

import daqcore.util._


trait COAccessType[A] {
  def dataType: CODataType[A]
  def isReadable: Boolean = false
  def isWriteable: Boolean = false
  def isConst: Boolean = false
}


trait COReadable[A] extends COAccessType[A] {
  override def isReadable = true
  def dataType: CODataType[A]

  def decode(iterator: ByteIterator) = dataType.decode(iterator)
  def decode(bytes: ByteString): A = decode(bytes.iterator)
}


trait COWritable[A] extends COAccessType[A] {
  override def isWriteable = true
  def dataType: CODataType[A]

  def encode(builder: ByteStringBuilder, value: A) = dataType.encode(builder, value)
  def encode(value: A): ByteString = encode(ByteString.newBuilder, value).result
}


case class COTypedRO[A](dataType: CODataType[A]) extends COReadable[A]

case class COTypedWO[A](dataType: CODataType[A]) extends COWritable[A]

case class COTypedRW[A](dataType: CODataType[A]) extends COReadable[A] with COWritable[A]

case class COTypedConst[A](dataType: CODataType[A]) extends COReadable[A] {
  override def isConst = true
}
