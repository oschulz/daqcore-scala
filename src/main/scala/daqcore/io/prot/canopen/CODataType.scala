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


abstract class CODataType[A] {
  implicit final def nioByteOrder = java.nio.ByteOrder.LITTLE_ENDIAN

  def id: Int
  def name: String

  def encode(builder: ByteStringBuilder, value: A): ByteStringBuilder
  def decode(iterator: ByteIterator): A

  def ro = COTypedRO(this)
  def wo = COTypedWO(this)
  def rw = COTypedRW(this)
  def const = COTypedConst(this)
}

case object COBoolean extends CODataType[Boolean]  {
  def id = 0x01
  def name = "Boolean"
  def encode(builder: ByteStringBuilder, value: Boolean) = builder.putByte(if (value) 1.toByte else 0.toByte)
  def decode(iterator: ByteIterator): Boolean = if (iterator.getByte > 0) true else false
}

case object COInteger8 extends CODataType[Byte]  {
  def id = 0x02
  def name = "Integer8"
  def encode(builder: ByteStringBuilder, value: Byte) = builder.putByte(value)
  def decode(iterator: ByteIterator): Byte = iterator.getByte
}

case object COInteger16 extends CODataType[Short]  {
  def id = 0x03
  def name = "Integer16"
  def encode(builder: ByteStringBuilder, value: Short) = builder.putShort(value)
  def decode(iterator: ByteIterator): Short = iterator.getShort
}

case object COInteger32 extends CODataType[Int]  {
  def id = 0x04
  def name = "Integer32"
  def encode(builder: ByteStringBuilder, value: Int) = builder.putInt(value)
  def decode(iterator: ByteIterator): Int = iterator.getInt
}

case object COUnsigned8 extends CODataType[Short]  {
  def id = 0x05
  def name = "Unsigned8"
  def encode(builder: ByteStringBuilder, value: Short) = { val UnsignedByte(u) = value; builder.putByte(u) }
  def decode(iterator: ByteIterator): Short = UnsignedByte(iterator.getByte)
}

case object COUnsigned16 extends CODataType[Int]  {
  def id = 0x06
  def name = "Unsigned16"
  def encode(builder: ByteStringBuilder, value: Int) = { val UnsignedShort(u) = value; builder.putShort(u) }
  def decode(iterator: ByteIterator): Int = UnsignedShort(iterator.getShort)
}

case object COUnsigned32 extends CODataType[Long]  {
  def id = 0x07
  def name = "Unsigned32"
  def encode(builder: ByteStringBuilder, value: Long) = { val UnsignedInt(u) = value; builder.putInt(u) }
  def decode(iterator: ByteIterator): Long = UnsignedInt(iterator.getInt)
}

case object COReal32 extends CODataType[Float]  {
  def id = 0x08
  def name = "Real32"
  def encode(builder: ByteStringBuilder, value: Float) = builder.putFloat(value)
  def decode(iterator: ByteIterator): Float = iterator.getFloat
}

case object COVisibleString extends CODataType[String] {
  def encoding = "ASCII"
  def id = 0x09
  def name = "Visible_String"
  def encode(builder: ByteStringBuilder, value: String) = builder ++= ByteString(value, encoding)
  def decode(iterator: ByteIterator): String = iterator.toByteString.decodeString(encoding)
}

case object COOctetString extends CODataType[ByteString] {
  def id = 0x0A
  def name = "Visible_String"
  def encode(builder: ByteStringBuilder, value: ByteString) = builder ++= value
  def decode(iterator: ByteIterator): ByteString = iterator.toByteString
}
