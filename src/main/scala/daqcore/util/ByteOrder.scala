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

sealed abstract class ByteOrder {
  def toNIO: NIOByteOrder

  def fromBytes[A <: AnyVal : ClassManifest](bytes: Seq[Byte]) : Seq[A] = {
    val mf = classManifest[A]
    if (mf == classManifest[Byte]) bytes.asInstanceOf[Seq[A]]
    else {
      val byteArray = bytes.toArray
      val byteBuf = NIOByteBuffer.wrap(byteArray)
      if (this == LittleEndian) byteBuf.order(NIOByteOrder.LITTLE_ENDIAN)

      if (mf == classManifest[Char]) {
        val buf = byteBuf.asCharBuffer
        Array.fill(buf.limit){buf.get}.toSeq.asInstanceOf[Seq[A]]
      }
      else if (mf == classManifest[Short]) {
        val buf = byteBuf.asShortBuffer
        Array.fill(buf.limit){buf.get}.toSeq.asInstanceOf[Seq[A]]
      }
      else if (mf == classManifest[Int]) {
        val buf = byteBuf.asIntBuffer
        Array.fill(buf.limit){buf.get}.toSeq.asInstanceOf[Seq[A]]
      }
      else if (mf == classManifest[Long]) {
        val buf = byteBuf.asLongBuffer
        Array.fill(buf.limit){buf.get}.toSeq.asInstanceOf[Seq[A]]
      }
      else if (mf == classManifest[Float]) {
        val buf = byteBuf.asFloatBuffer
        Array.fill(buf.limit){buf.get}.toSeq.asInstanceOf[Seq[A]]
      }
      else if (mf == classManifest[Double]) {
        val buf = byteBuf.asDoubleBuffer
        Array.fill(buf.limit){buf.get}.toSeq.asInstanceOf[Seq[A]]
      }
      else throw new IllegalArgumentException("convertTo() does not support " + mf)
    }
  }


  def toBytes[A <: AnyVal : ClassManifest](seq: Seq[A]) : Seq[Byte] = {
    val mf = classManifest[A]
    if (mf == classManifest[Byte]) seq.asInstanceOf[Seq[Byte]]
    else {
      val byteBuf = NIOByteBuffer.allocate(seq.size * sizeOf[A])
      if (this == LittleEndian) byteBuf.order(NIOByteOrder.LITTLE_ENDIAN)
      assert (byteBuf.hasArray)
      val byteArray = byteBuf.array
      assert (byteArray.size == seq.size * sizeOf[A])

      if (mf == classManifest[Char]) {
        val buf = byteBuf.asCharBuffer
        assert(buf.capacity == seq.size)
        seq.asInstanceOf[Seq[Char]] foreach { buf.put(_) }
      }
      else if (mf == classManifest[Short]) {
        val buf = byteBuf.asShortBuffer
        assert(buf.capacity == seq.size)
        seq.asInstanceOf[Seq[Short]] foreach { buf.put(_) }
      }
      else if (mf == classManifest[Int]) {
        val buf = byteBuf.asIntBuffer
        assert(buf.capacity == seq.size)
        seq.asInstanceOf[Seq[Int]] foreach { buf.put(_) }
      }
      else if (mf == classManifest[Long]) {
        val buf = byteBuf.asLongBuffer
        assert(buf.capacity == seq.size)
        seq.asInstanceOf[Seq[Long]] foreach { buf.put(_) }
      }
      else if (mf == classManifest[Float]) {
        val buf = byteBuf.asFloatBuffer
        assert(buf.capacity == seq.size)
        seq.asInstanceOf[Seq[Float]] foreach { buf.put(_) }
      }
      else if (mf == classManifest[Double]) {
        val buf = byteBuf.asDoubleBuffer
        assert(buf.capacity == seq.size)
        seq.asInstanceOf[Seq[Double]] foreach { buf.put(_) }
      }
      
      byteArray.toSeq
    }
  }
}


object ByteOrder {
  def apply(nioBO: NIOByteOrder) = nioBO match {
    case NIOByteOrder.BIG_ENDIAN => BigEndian
    case java.nio.ByteOrder.LITTLE_ENDIAN => LittleEndian
    case _ => throw new IllegalArgumentException("Byte order %s not supported".format(nioBO))
  }
  
  val native = ByteOrder(NIOByteOrder.nativeOrder())
}


case object BigEndian extends ByteOrder {
  def toNIO = NIOByteOrder.BIG_ENDIAN 
}


case object LittleEndian extends ByteOrder {
  def toNIO = java.nio.ByteOrder.LITTLE_ENDIAN
}
