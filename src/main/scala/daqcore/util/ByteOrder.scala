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
  def nioByteOrder: NIOByteOrder

  protected def getRawBytes(source: ByteSeqIterator, length: Int): Array[Byte] = {
    // relies on source.duplicate returning (source, ...) - for some
    // reason, using duplicate/drop/take here make the bulk get methods
    // run significantly faster than using source.copyToArray
    val (src, it) = source.duplicate
    val rest = src.drop(length)
    assert(rest eq source)
    
    val array = it.take(length).toArray
    if (array.length < length) { assert(it.isEmpty); it.next() }
    array
  }

  protected def getToNIOByteBuffer(source: ByteSeqIterator, length: Int): NIOByteBuffer = {
    val byteArray = getRawBytes(source, length)
    val byteBuf = NIOByteBuffer.wrap(byteArray)
    byteBuf.order(nioByteOrder)
    byteBuf
  }

  protected def newNIOByteBuffer(length: Int): NIOByteBuffer = {
      val byteBuf = NIOByteBuffer.allocate(length)
      byteBuf.order(nioByteOrder)
      assert (byteBuf.hasArray)
      byteBuf
  }
  
  override def putBytes(target: ByteSeqBuilder, xs: ArrayIterator[Byte]): Unit = {
    target ++= xs.toArray
  }

  override def putShorts(target: ByteSeqBuilder, xs: ArrayIterator[Short]): Unit = {
    val nBytes = xs.size * sizeOf[Short]
    val byteBuf = newNIOByteBuffer(nBytes)
    val buf = byteBuf.asShortBuffer
    assert(buf.capacity == xs.size)
    buf.put(xs.toArray)
    val byteArray = byteBuf.array
    assert (byteArray.length == nBytes)
    target ++= byteArray
  }

  override def putInts(target: ByteSeqBuilder, xs: ArrayIterator[Int]): Unit = {
    val nBytes = xs.size * sizeOf[Int]
    val byteBuf = newNIOByteBuffer(nBytes)
    val buf = byteBuf.asIntBuffer
    assert(buf.capacity == xs.size)
    buf.put(xs.toArray)
    val byteArray = byteBuf.array
    assert (byteArray.length == nBytes)
    target ++= byteArray
  }
  
  override def putLongs(target: ByteSeqBuilder, xs: ArrayIterator[Long]): Unit = {
    val nBytes = xs.size * sizeOf[Long]
    val byteBuf = newNIOByteBuffer(nBytes)
    val buf = byteBuf.asLongBuffer
    assert(buf.capacity == xs.size)
    buf.put(xs.toArray)
    val byteArray = byteBuf.array
    assert (byteArray.length == nBytes)
    target ++= byteArray
  }

  override def putFloats(target: ByteSeqBuilder, xs: ArrayIterator[Float]): Unit = {
    val nBytes = xs.size * sizeOf[Float]
    val byteBuf = newNIOByteBuffer(nBytes)
    val buf = byteBuf.asFloatBuffer
    assert(buf.capacity == xs.size)
    buf.put(xs.toArray)
    val byteArray = byteBuf.array
    assert (byteArray.length == nBytes)
    target ++= byteArray
  }

  override def putDoubles(target: ByteSeqBuilder, xs: ArrayIterator[Double]): Unit = {
    val nBytes = xs.size * sizeOf[Double]
    val byteBuf = newNIOByteBuffer(nBytes)
    val buf = byteBuf.asDoubleBuffer
    assert(buf.capacity == xs.size)
    buf.put(xs.toArray)
    val byteArray = byteBuf.array
    assert (byteArray.length == nBytes)
    target ++= byteArray
  }


  override def getBytes(source: ByteSeqIterator, length: Int): ArrayVec[Byte] = {
    ArrayVec.wrap(getRawBytes(source, length))
  }

  override def getShorts(source: ByteSeqIterator, length: Int): ArrayVec[Short] = {
    val buf = getToNIOByteBuffer(source, length * sizeOf[Short]).asShortBuffer
    val array = Array.ofDim[Short](length)
    buf.get(array)
    ArrayVec.wrap(array)
  }

  override def getInts(source: ByteSeqIterator, length: Int): ArrayVec[Int] = {
    val buf = getToNIOByteBuffer(source, length * sizeOf[Int]).asIntBuffer
    val array = Array.ofDim[Int](length)
    buf.get(array)
    ArrayVec.wrap(array)
  }

  override def getLongs(source: ByteSeqIterator, length: Int): ArrayVec[Long] = {
    val buf = getToNIOByteBuffer(source, length * sizeOf[Long]).asLongBuffer
    val array = Array.ofDim[Long](length)
    buf.get(array)
    ArrayVec.wrap(array)
  }

  override def getFloats(source: ByteSeqIterator, length: Int): ArrayVec[Float] = {
    val buf = getToNIOByteBuffer(source, length * sizeOf[Float]).asFloatBuffer
    val array = Array.ofDim[Float](length)
    buf.get(array)
    ArrayVec.wrap(array)
  }
  
  override def getDoubles(source: ByteSeqIterator, length: Int): ArrayVec[Double] = {
    val buf = getToNIOByteBuffer(source, length * sizeOf[Double]).asDoubleBuffer
    val array = Array.ofDim[Double](length)
    buf.get(array)
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
  def nioByteOrder = NIOByteOrder.BIG_ENDIAN

  def putByte(target: ByteSeqBuilder, x: Byte) = {
    target += x
  }

  def putShort(target: ByteSeqBuilder, x: Short) = {
     target += (x >>>  8).toByte
     target += (x >>>  0).toByte
  }

  def putInt(target: ByteSeqBuilder, x: Int) = {
     target += (x >>> 24).toByte
     target += (x >>> 16).toByte
     target += (x >>>  8).toByte
     target += (x >>>  0).toByte
  }

  def putLong(target: ByteSeqBuilder, x: Long) = {
     target += (x >>> 56).toByte
     target += (x >>> 48).toByte
     target += (x >>> 40).toByte
     target += (x >>> 32).toByte
     target += (x >>> 24).toByte
     target += (x >>> 16).toByte
     target += (x >>>  8).toByte
     target += (x >>>  0).toByte
  }


  def getByte(source: ByteSeqIterator) = {
    source.next()
  }
  
  def getShort(source: ByteSeqIterator) = {
     ( (source.next() & 0xff) <<  8
     | (source.next() & 0xff) <<  0
     ).toShort
  }

  def getInt(source: ByteSeqIterator) = {
     ( (source.next() & 0xff) << 24
     | (source.next() & 0xff) << 16
     | (source.next() & 0xff) <<  8
     | (source.next() & 0xff) <<  0
     )
  }

  def getLong(source: ByteSeqIterator) = {
     ( (source.next().toLong & 0xff) << 56
     | (source.next().toLong & 0xff) << 48
     | (source.next().toLong & 0xff) << 40
     | (source.next().toLong & 0xff) << 32
     | (source.next().toLong & 0xff) << 24
     | (source.next().toLong & 0xff) << 16
     | (source.next().toLong & 0xff) <<  8
     | (source.next().toLong & 0xff) <<  0
     )
  }
}



case object LittleEndian extends ByteOrder {
  def nioByteOrder = java.nio.ByteOrder.LITTLE_ENDIAN


  def putByte(target: ByteSeqBuilder, x: Byte) = {
    target += x
  }

  def putShort(target: ByteSeqBuilder, x: Short) = {
     target += (x >>>  0).toByte
     target += (x >>>  8).toByte
  }

  def putInt(target: ByteSeqBuilder, x: Int) = {
     target += (x >>>  0).toByte
     target += (x >>>  8).toByte
     target += (x >>> 16).toByte
     target += (x >>> 24).toByte
  }

  def putLong(target: ByteSeqBuilder, x: Long) = {
     target += (x >>>  0).toByte
     target += (x >>>  8).toByte
     target += (x >>> 16).toByte
     target += (x >>> 24).toByte
     target += (x >>> 32).toByte
     target += (x >>> 40).toByte
     target += (x >>> 48).toByte
     target += (x >>> 56).toByte
  }


  def getByte(source: ByteSeqIterator) = {
    source.next()
  }
  
  def getShort(source: ByteSeqIterator) = {
     ( (source.next() & 0xff) <<  0
     | (source.next() & 0xff) <<  8
     ).toShort
  }

  def getInt(source: ByteSeqIterator) = {
     ( (source.next() & 0xff) <<  0
     | (source.next() & 0xff) <<  8
     | (source.next() & 0xff) << 16
     | (source.next() & 0xff) << 24
     )
  }

  def getLong(source: ByteSeqIterator) = {
     ( (source.next().toLong & 0xff) <<  0
     | (source.next().toLong & 0xff) <<  8
     | (source.next().toLong & 0xff) << 16
     | (source.next().toLong & 0xff) << 24
     | (source.next().toLong & 0xff) << 32
     | (source.next().toLong & 0xff) << 40
     | (source.next().toLong & 0xff) << 48
     | (source.next().toLong & 0xff) << 56
     )
  }
}
