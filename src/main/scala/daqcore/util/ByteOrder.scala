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

sealed trait ByteOrder {
  def toNIO: NIOByteOrder

  def fromBytes[A <: AnyVal : ClassManifest](bytes: ByteSeq) : ArrayVec[A] = {
    val mf = classManifest[A]
    if (mf == classManifest[Byte]) bytes.asInstanceOf[ArrayVec[A]]
    else {
      val byteArray = bytes.toArray
      val byteBuf = NIOByteBuffer.wrap(byteArray)
      if (this == LittleEndian) byteBuf.order(NIOByteOrder.LITTLE_ENDIAN)

      if (mf == classManifest[Byte]) {
        ArrayVec.wrap(bytes.toArray.asInstanceOf[Array[A]])
      }
      else if (mf == classManifest[Short]) {
        val buf = byteBuf.asShortBuffer
        val array = Array.ofDim[Short](buf.limit)
        buf.get(array)
        ArrayVec.wrap(array.asInstanceOf[Array[A]])
      }
      else if (mf == classManifest[Int]) {
        val buf = byteBuf.asIntBuffer
        val array = Array.ofDim[Int](buf.limit)
        buf.get(array)
        ArrayVec.wrap(array.asInstanceOf[Array[A]])
      }
      else if (mf == classManifest[Long]) {
        val buf = byteBuf.asLongBuffer
        val array = Array.ofDim[Long](buf.limit)
        buf.get(array)
        ArrayVec.wrap(array.asInstanceOf[Array[A]])
      }
      else if (mf == classManifest[Float]) {
        val buf = byteBuf.asFloatBuffer
        val array = Array.ofDim[Float](buf.limit)
        buf.get(array)
        ArrayVec.wrap(array.asInstanceOf[Array[A]])
      }
      else if (mf == classManifest[Double]) {
        val buf = byteBuf.asDoubleBuffer
        val array = Array.ofDim[Double](buf.limit)
        buf.get(array)
        ArrayVec.wrap(array.asInstanceOf[Array[A]])
      }
      else throw new UnsupportedOperationException("convertTo() does not support " + mf)
    }
  }


  def toBytes[A <: AnyVal : ClassManifest](seq: ArrayVec[A]) : ByteSeq = {
    val mf = classManifest[A]
    if (mf == classManifest[Byte]) seq.asInstanceOf[ByteSeq]
    else {
      val byteBuf = NIOByteBuffer.allocate(seq.size * sizeOf[A])
      if (this == LittleEndian) byteBuf.order(NIOByteOrder.LITTLE_ENDIAN)
      assert (byteBuf.hasArray)
      val byteArray = byteBuf.array
      assert (byteArray.size == seq.size * sizeOf[A])

      if (mf == classManifest[Byte]) {
        byteBuf.put(seq.asInstanceOf[ArrayVec[Byte]].toArray)
      }
      else if (mf == classManifest[Short]) {
        val buf = byteBuf.asShortBuffer
        assert(buf.capacity == seq.size)
        buf.put(seq.asInstanceOf[ArrayVec[Short]].toArray)
      }
      else if (mf == classManifest[Int]) {
        val buf = byteBuf.asIntBuffer
        assert(buf.capacity == seq.size)
        buf.put(seq.asInstanceOf[ArrayVec[Int]].toArray)
      }
      else if (mf == classManifest[Long]) {
        val buf = byteBuf.asLongBuffer
        assert(buf.capacity == seq.size)
        buf.put(seq.asInstanceOf[ArrayVec[Long]].toArray)
      }
      else if (mf == classManifest[Float]) {
        val buf = byteBuf.asFloatBuffer
        assert(buf.capacity == seq.size)
        buf.put(seq.asInstanceOf[ArrayVec[Float]].toArray)
      }
      else if (mf == classManifest[Double]) {
        val buf = byteBuf.asDoubleBuffer
        assert(buf.capacity == seq.size)
        buf.put(seq.asInstanceOf[ArrayVec[Double]].toArray)
      }
      
      ByteSeq.wrap(byteArray)
    }
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



case object BigEndian extends ByteOrder with ValEncoding {
  def toNIO = NIOByteOrder.BIG_ENDIAN

  def putByte(x: Byte)(implicit target: ByteSeqBuilder) = {
    target += x
  }

  def putShort(x: Short)(implicit target: ByteSeqBuilder) = {
     target += (x >>>  8).toByte
     target += (x >>>  0).toByte
  }

  def putInt(x: Int)(implicit target: ByteSeqBuilder) = {
     target += (x >>> 24).toByte
     target += (x >>> 16).toByte
     target += (x >>>  8).toByte
     target += (x >>>  0).toByte
  }

  def putLong(x: Long)(implicit target: ByteSeqBuilder) = {
     target += (x >>> 56).toByte
     target += (x >>> 48).toByte
     target += (x >>> 40).toByte
     target += (x >>> 32).toByte
     target += (x >>> 24).toByte
     target += (x >>> 16).toByte
     target += (x >>>  8).toByte
     target += (x >>>  0).toByte
  }


  def getByte()(implicit source: ByteSeqIterator) = {
    source.next()
  }
  
  def getShort()(implicit source: ByteSeqIterator) = {
     ( (source.next() & 0xff) <<  8
     | (source.next() & 0xff) <<  0
     ).toShort
  }

  def getInt()(implicit source: ByteSeqIterator) = {
     ( (source.next() & 0xff) << 24
     | (source.next() & 0xff) << 16
     | (source.next() & 0xff) <<  8
     | (source.next() & 0xff) <<  0
     )
  }

  def getLong()(implicit source: ByteSeqIterator) = {
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



case object LittleEndian extends ByteOrder with ValEncoding {
  def toNIO = java.nio.ByteOrder.LITTLE_ENDIAN


  def putByte(x: Byte)(implicit target: ByteSeqBuilder) = {
    target += x
  }

  def putShort(x: Short)(implicit target: ByteSeqBuilder) = {
     target += (x >>>  0).toByte
     target += (x >>>  8).toByte
  }

  def putInt(x: Int)(implicit target: ByteSeqBuilder) = {
     target += (x >>>  0).toByte
     target += (x >>>  8).toByte
     target += (x >>> 16).toByte
     target += (x >>> 24).toByte
  }

  def putLong(x: Long)(implicit target: ByteSeqBuilder) = {
     target += (x >>>  0).toByte
     target += (x >>>  8).toByte
     target += (x >>> 16).toByte
     target += (x >>> 24).toByte
     target += (x >>> 32).toByte
     target += (x >>> 40).toByte
     target += (x >>> 48).toByte
     target += (x >>> 56).toByte
  }


  def getByte()(implicit source: ByteSeqIterator) = {
    source.next()
  }
  
  def getShort()(implicit source: ByteSeqIterator) = {
     ( (source.next() & 0xff) <<  0
     | (source.next() & 0xff) <<  8
     ).toShort
  }

  def getInt()(implicit source: ByteSeqIterator) = {
     ( (source.next() & 0xff) <<  0
     | (source.next() & 0xff) <<  8
     | (source.next() & 0xff) << 16
     | (source.next() & 0xff) << 24
     )
  }

  def getLong()(implicit source: ByteSeqIterator) = {
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
