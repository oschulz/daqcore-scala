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


trait ValEncoding {
  def putByte(target: ByteSeqBuilder, x: Byte): Unit
  def putShort(target: ByteSeqBuilder, x: Short): Unit
  def putInt(target: ByteSeqBuilder, x: Int): Unit
  def putLong(target: ByteSeqBuilder, x: Long): Unit
  
  def putFloat(target: ByteSeqBuilder, x: Float): Unit =
    putInt(target, java.lang.Float.floatToRawIntBits(x))
  
  def putDouble(target: ByteSeqBuilder, x: Double): Unit =
    putLong(target, java.lang.Double.doubleToRawLongBits(x))

  def getByte(source: ByteSeqIterator): Byte
  def getShort(source: ByteSeqIterator): Short
  def getInt(source: ByteSeqIterator): Int
  def getLong(source: ByteSeqIterator): Long

  def getFloat(source: ByteSeqIterator): Float =
    java.lang.Float.intBitsToFloat(getInt(source))

  def getDouble(source: ByteSeqIterator): Double =
    java.lang.Double.longBitsToDouble(getLong(source))


  def putBytes(target: ByteSeqBuilder, xs: ArrayIterator[Byte]): Unit =
    for (x <- xs) putByte(target, x)
  
  def putShorts(target: ByteSeqBuilder, xs: ArrayIterator[Short]): Unit =
    for (x <- xs) putShort(target, x)

  def putInts(target: ByteSeqBuilder, xs: ArrayIterator[Int]): Unit =
    for (x <- xs) putInt(target, x)
  
  def putLongs(target: ByteSeqBuilder, xs: ArrayIterator[Long]): Unit =
    for (x <- xs) putLong(target, x)

  def putFloats(target: ByteSeqBuilder, xs: ArrayIterator[Float]): Unit =
    for (x <- xs) putFloat(target, x)
  
  def putDoubles(target: ByteSeqBuilder, xs: ArrayIterator[Double]): Unit =
    for (x <- xs) putDouble(target, x)


  def putBytes(target: ByteSeqBuilder, xs: ArrayVec[Byte]): Unit =
    putBytes(target, xs.iterator)
  
  def putShorts(target: ByteSeqBuilder, xs: ArrayVec[Short]): Unit =
    putShorts(target, xs.iterator)

  def putInts(target: ByteSeqBuilder, xs: ArrayVec[Int]): Unit =
    putInts(target, xs.iterator)
  
  def putLongs(target: ByteSeqBuilder, xs: ArrayVec[Long]): Unit =
    putLongs(target, xs.iterator)

  def putFloats(target: ByteSeqBuilder, xs: ArrayVec[Float]): Unit =
    putFloats(target, xs.iterator)
  
  def putDoubles(target: ByteSeqBuilder, xs: ArrayVec[Double]): Unit =
    putDoubles(target, xs.iterator)


  def getBytes(source: ByteSeqIterator, length: Int): ArrayVec[Byte] = {
    val target = Array.ofDim[Byte](length)
    for (i <- Range(0, length)) target(i) = getByte(source)
    ArrayVec.wrap(target)
  }

  def getShorts(source: ByteSeqIterator, length: Int): ArrayVec[Short] = {
    val target = Array.ofDim[Short](length)
    for (i <- Range(0, length)) target(i) = getShort(source)
    ArrayVec.wrap(target)
  }

  def getInts(source: ByteSeqIterator, length: Int): ArrayVec[Int] = {
    val target = Array.ofDim[Int](length)
    for (i <- Range(0, length)) target(i) = getInt(source)
    ArrayVec.wrap(target)
  }

  def getLongs(source: ByteSeqIterator, length: Int): ArrayVec[Long] = {
    val target = Array.ofDim[Long](length)
    for (i <- Range(0, length)) target(i) = getLong(source)
    ArrayVec.wrap(target)
  }

  def getFloats(source: ByteSeqIterator, length: Int): ArrayVec[Float] = {
    val target = Array.ofDim[Float](length)
    for (i <- Range(0, length)) target(i) = getFloat(source)
    ArrayVec.wrap(target)
  }
  
  def getDoubles(source: ByteSeqIterator, length: Int): ArrayVec[Double] = {
    val target = Array.ofDim[Double](length)
    for (i <- Range(0, length)) target(i) = getDouble(source)
    ArrayVec.wrap(target)
  }
}
