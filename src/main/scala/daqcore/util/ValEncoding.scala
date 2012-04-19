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
  def putByte(target: GenericByteSeqBuilder, x: Byte): Unit
  def putShort(target: GenericByteSeqBuilder, x: Short): Unit
  def putInt(target: GenericByteSeqBuilder, x: Int): Unit
  def putLong(target: GenericByteSeqBuilder, x: Long): Unit
  
  def putFloat(target: GenericByteSeqBuilder, x: Float): Unit =
    putInt(target, java.lang.Float.floatToRawIntBits(x))
  
  def putDouble(target: GenericByteSeqBuilder, x: Double): Unit =
    putLong(target, java.lang.Double.doubleToRawLongBits(x))

  def getByte(source: GenericByteSeqIterator): Byte
  def getShort(source: GenericByteSeqIterator): Short
  def getInt(source: GenericByteSeqIterator): Int
  def getLong(source: GenericByteSeqIterator): Long

  def getFloat(source: GenericByteSeqIterator): Float =
    java.lang.Float.intBitsToFloat(getInt(source))

  def getDouble(source: GenericByteSeqIterator): Double =
    java.lang.Double.longBitsToDouble(getLong(source))


  def putBytes(target: GenericByteSeqBuilder, xs: ArrayIterator[Byte]): Unit =
    for (x <- xs) putByte(target, x)
  
  def putShorts(target: GenericByteSeqBuilder, xs: ArrayIterator[Short]): Unit =
    for (x <- xs) putShort(target, x)

  def putInts(target: GenericByteSeqBuilder, xs: ArrayIterator[Int]): Unit =
    for (x <- xs) putInt(target, x)
  
  def putLongs(target: GenericByteSeqBuilder, xs: ArrayIterator[Long]): Unit =
    for (x <- xs) putLong(target, x)

  def putFloats(target: GenericByteSeqBuilder, xs: ArrayIterator[Float]): Unit =
    for (x <- xs) putFloat(target, x)
  
  def putDoubles(target: GenericByteSeqBuilder, xs: ArrayIterator[Double]): Unit =
    for (x <- xs) putDouble(target, x)


  def putBytes(target: GenericByteSeqBuilder, xs: ArrayVec[Byte]): Unit =
    putBytes(target, xs.iterator)
  
  def putShorts(target: GenericByteSeqBuilder, xs: ArrayVec[Short]): Unit =
    putShorts(target, xs.iterator)

  def putInts(target: GenericByteSeqBuilder, xs: ArrayVec[Int]): Unit =
    putInts(target, xs.iterator)
  
  def putLongs(target: GenericByteSeqBuilder, xs: ArrayVec[Long]): Unit =
    putLongs(target, xs.iterator)

  def putFloats(target: GenericByteSeqBuilder, xs: ArrayVec[Float]): Unit =
    putFloats(target, xs.iterator)
  
  def putDoubles(target: GenericByteSeqBuilder, xs: ArrayVec[Double]): Unit =
    putDoubles(target, xs.iterator)


  def getBytes(source: GenericByteSeqIterator, length: Int): ArrayVec[Byte] = {
    val target = Array.ofDim[Byte](length)
    for (i <- Range(0, length)) target(i) = getByte(source)
    ArrayVec.wrap(target)
  }

  def getShorts(source: GenericByteSeqIterator, length: Int): ArrayVec[Short] = {
    val target = Array.ofDim[Short](length)
    for (i <- Range(0, length)) target(i) = getShort(source)
    ArrayVec.wrap(target)
  }

  def getInts(source: GenericByteSeqIterator, length: Int): ArrayVec[Int] = {
    val target = Array.ofDim[Int](length)
    for (i <- Range(0, length)) target(i) = getInt(source)
    ArrayVec.wrap(target)
  }

  def getLongs(source: GenericByteSeqIterator, length: Int): ArrayVec[Long] = {
    val target = Array.ofDim[Long](length)
    for (i <- Range(0, length)) target(i) = getLong(source)
    ArrayVec.wrap(target)
  }

  def getFloats(source: GenericByteSeqIterator, length: Int): ArrayVec[Float] = {
    val target = Array.ofDim[Float](length)
    for (i <- Range(0, length)) target(i) = getFloat(source)
    ArrayVec.wrap(target)
  }
  
  def getDoubles(source: GenericByteSeqIterator, length: Int): ArrayVec[Double] = {
    val target = Array.ofDim[Double](length)
    for (i <- Range(0, length)) target(i) = getDouble(source)
    ArrayVec.wrap(target)
  }

  def fromBytes[A <: AnyVal : ClassManifest](bytes: GenericByteSeq): ArrayVec[A] = {
    val mf = classManifest[A]
    if (mf == classManifest[Byte]) getBytes(bytes.iterator, bytes.length / sizeOf[Byte]).asInstanceOf[ArrayVec[A]]
    else if (mf == classManifest[Short]) getShorts(bytes.iterator, bytes.length / sizeOf[Short]).asInstanceOf[ArrayVec[A]]
    else if (mf == classManifest[Int]) getInts(bytes.iterator, bytes.length / sizeOf[Int]).asInstanceOf[ArrayVec[A]]
    else if (mf == classManifest[Long]) getLongs(bytes.iterator, bytes.length / sizeOf[Long]).asInstanceOf[ArrayVec[A]]
    else if (mf == classManifest[Float]) getFloats(bytes.iterator, bytes.length / sizeOf[Float]).asInstanceOf[ArrayVec[A]]
    else if (mf == classManifest[Double]) getDoubles(bytes.iterator, bytes.length / sizeOf[Double]).asInstanceOf[ArrayVec[A]]
    else throw new UnsupportedOperationException("ByteOrder.fromBytes() does not support " + mf)
  }


  def toBytes[A <: AnyVal : ClassManifest](seq: ArrayVec[A]): ByteSeq = {
    val mf = classManifest[A]
    val builder = ByteSeqBuilder(seq.length * sizeOf[A])
    if (mf == classManifest[Byte]) putBytes(builder, seq.asInstanceOf[ArrayVec[Byte]])
    else if (mf == classManifest[Short]) putShorts(builder, seq.asInstanceOf[ArrayVec[Short]])
    else if (mf == classManifest[Int]) putInts(builder, seq.asInstanceOf[ArrayVec[Int]])
    else if (mf == classManifest[Long]) putLongs(builder, seq.asInstanceOf[ArrayVec[Long]])
    else if (mf == classManifest[Float]) putFloats(builder, seq.asInstanceOf[ArrayVec[Float]])
    else if (mf == classManifest[Double]) putDoubles(builder, seq.asInstanceOf[ArrayVec[Double]])
    else throw new UnsupportedOperationException("ByteOrder.fromBytes() does not support " + mf)
    builder.result
  }
}
