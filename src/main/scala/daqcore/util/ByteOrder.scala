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

import java.nio.{ByteOrder => NIOByteOrder, ByteBuffer => NIOByteBuffer}

sealed trait ByteOrder extends ValEncoding {
  implicit def nioByteOrder: NIOByteOrder

  def putByte(target: ByteStringBuilder, x: Byte) = target.putByte(x)
  
  def putShort(target: ByteStringBuilder, x: Short) = target.putShort(x)

  def putInt(target: ByteStringBuilder, x: Int) = target.putInt(x)

  def putLong(target: ByteStringBuilder, x: Long) = target.putLong(x)

  override def putFloat(target: ByteStringBuilder, x: Float) = target.putFloat(x)

  override def putDouble(target: ByteStringBuilder, x: Double) = target.putDouble(x)


  override def putBytes(target: ByteStringBuilder, xs: ArrayIterator[Byte]): Unit =
    target.putBytes(xs.internalArray, xs.internalFrom, xs.internalUntil - xs.internalFrom)

  override def putShorts(target: ByteStringBuilder, xs: ArrayIterator[Short]): Unit =
    target.putShorts(xs.internalArray, xs.internalFrom, xs.internalUntil - xs.internalFrom)

  override def putInts(target: ByteStringBuilder, xs: ArrayIterator[Int]): Unit =
    target.putInts(xs.internalArray, xs.internalFrom, xs.internalUntil - xs.internalFrom)

  override def putLongs(target: ByteStringBuilder, xs: ArrayIterator[Long]): Unit =
    target.putLongs(xs.internalArray, xs.internalFrom, xs.internalUntil - xs.internalFrom)

  override def putFloats(target: ByteStringBuilder, xs: ArrayIterator[Float]): Unit =
    target.putFloats(xs.internalArray, xs.internalFrom, xs.internalUntil - xs.internalFrom)
  
  override def putDoubles(target: ByteStringBuilder, xs: ArrayIterator[Double]): Unit =
    target.putDoubles(xs.internalArray, xs.internalFrom, xs.internalUntil - xs.internalFrom)


  def getByte(source: ByteIterator) = source.getByte
  
  def getShort(source: ByteIterator) = source.getShort

  def getInt(source: ByteIterator) = source.getInt

  def getLong(source: ByteIterator) = source.getLong

  override def getFloat(source: ByteIterator) = source.getFloat

  override def getDouble(source: ByteIterator) = source.getDouble


  override def getBytes(source: ByteIterator, length: Int): ArrayVec[Byte] = {
    val array = Array.ofDim[Byte](length)
    source.getBytes(array)
    ArrayVec.wrap(array)
  }

  override def getShorts(source: ByteIterator, length: Int): ArrayVec[Short] = {
    val array = Array.ofDim[Short](length)
    source.getShorts(array)
    ArrayVec.wrap(array)
  }

  override def getInts(source: ByteIterator, length: Int): ArrayVec[Int] = {
    val array = Array.ofDim[Int](length)
    source.getInts(array)
    ArrayVec.wrap(array)
  }

  override def getLongs(source: ByteIterator, length: Int): ArrayVec[Long] = {
    val array = Array.ofDim[Long](length)
    source.getLongs(array)
    ArrayVec.wrap(array)
  }

  override def getFloats(source: ByteIterator, length: Int): ArrayVec[Float] = {
    val array = Array.ofDim[Float](length)
    source.getFloats(array)
    ArrayVec.wrap(array)
  }

  override def getDoubles(source: ByteIterator, length: Int): ArrayVec[Double] = {
    val array = Array.ofDim[Double](length)
    source.getDoubles(array)
    ArrayVec.wrap(array)
  }
}


object ByteOrder {
  def apply(nioBO: NIOByteOrder) = nioBO match {
    case NIOByteOrder.BIG_ENDIAN => BigEndian
    case java.nio.ByteOrder.LITTLE_ENDIAN => LittleEndian
    case _ => throw new UnsupportedOperationException("Byte order %s not supported".format(nioBO))
  }
  
  val native = ByteOrder(NIOByteOrder.nativeOrder())
}



case object BigEndian extends ByteOrder {
  implicit def nioByteOrder = NIOByteOrder.BIG_ENDIAN
}



case object LittleEndian extends ByteOrder {
  implicit def nioByteOrder = java.nio.ByteOrder.LITTLE_ENDIAN
}
