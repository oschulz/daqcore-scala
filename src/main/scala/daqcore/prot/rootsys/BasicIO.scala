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


package daqcore.prot.rootsys

import daqcore.util._

import java.util.UUID
import org.jboss.netty.buffer.{ChannelBuffer => NChannelBuffer, ChannelBuffers => NChannelBuffers}


trait BasicIO extends BasicInput with BasicOutput


abstract trait BasicInput {
  def readBoolean(): Boolean
  def readByte(): Byte
  def readShort(): Short
  def readInt(): Int
  def readLong(): Long
  def readFloat(): Float
  def readDouble(): Double
  def readString(): String
  def readUUID(): UUID

  def readArray[@specialized A: ClassManifest](): Array[A]
  
  def readSeq[A: ClassManifest](): ArrayVec[A] = readArray[A].toArrayVec
}


abstract trait BasicOutput {
  def writeBoolean(x: Boolean): Unit
  def writeByte(x: Byte): Unit
  def writeShort(x: Short): Unit
  def writeInt(x: Int): Unit
  def writeLong(x: Long): Unit
  def writeFloat(x: Float): Unit
  def writeDouble(x: Double): Unit
  def writeString(x: String): Unit
  def writeUUID(x: UUID): Unit

  def writeArray[@specialized A: ClassManifest](array: Array[A]): Unit
  
  def writeSeq[A: ClassManifest](seq: Seq[A]): Unit = writeArray(seq.toArray)
}


class BufferIO(val buffer: NChannelBuffer) extends BasicIO {
  def clear(): Unit = buffer.clear()

  def toArray: Array[Byte] = {
    val a = Array.ofDim[Byte](buffer.writerIndex)
    buffer.getBytes(0, a, 0, a.length)
    a
  }


  final def readBoolean() = (buffer.readByte() > 0)
  final def readByte() = buffer.readByte()
  final def readShort() = buffer.readShort()
  final def readInt() = buffer.readInt()
  final def readLong() = buffer.readLong()
  final def readFloat() = buffer.readFloat()
  final def readDouble() = buffer.readDouble()

  final def readString() = {
    val length = {
      val l = readByte()
      if (l < 0xff) l else readInt()
    }
    val a = Array.ofDim[Byte](length)
    buffer.readBytes(a)
    new String(a)
  }

  final def readUUID() = {
    val mostSigBits = readLong()
    val leastSigBits = readLong()
    new UUID(mostSigBits, leastSigBits)
  }

  final def writeBoolean(x: Boolean) = buffer.writeByte(if (x) 1 else 0)
  final def writeByte(x: Byte) = buffer.writeByte(x)
  final def writeShort(x: Short) = buffer.writeShort(x)
  final def writeInt(x: Int) = buffer.writeInt(x)
  final def writeLong(x: Long) = buffer.writeLong(x)
  final def writeFloat(x: Float) = buffer.writeFloat(x)
  final def writeDouble(x: Double) = buffer.writeDouble(x)

  final def writeString(x: String) = {
    if (x.length < 0xff) writeByte(x.length.toByte)
    else { writeByte(0xff.toByte); writeInt(x.length) }
    val a = x.getBytes()
    buffer.writeBytes(a)
  }

  final def writeUUID(x: UUID) = {
    val mostSigBits = x.getMostSignificantBits
    val leastSigBits = x.getLeastSignificantBits
    writeLong(mostSigBits)
    writeLong(leastSigBits)
  }

  final def writeArray[@specialized A: ClassManifest](array: Array[A]): Unit = {
    writeInt(array.length)

    if (array.isInstanceOf[Array[Boolean]]) {
      val a = array.asInstanceOf[Array[Boolean]]
      for (i <- 0 to a.length-1) writeBoolean(a(i))
    }
    else if (array.isInstanceOf[Array[Byte]]) {
      val a = array.asInstanceOf[Array[Byte]]
      buffer.writeBytes(a)
    }
    else if (array.isInstanceOf[Array[Short]]) {
      val a = array.asInstanceOf[Array[Short]]
      for (i <- 0 to a.length-1) buffer.writeShort(a(i))
    }
    else if (array.isInstanceOf[Array[Int]]) {
      val a = array.asInstanceOf[Array[Int]]
      for (i <- 0 to a.length-1) buffer.writeInt(a(i))
    }
    else if (array.isInstanceOf[Array[Long]]) {
      val a = array.asInstanceOf[Array[Long]]
      for (i <- 0 to a.length-1) buffer.writeLong(a(i))
    }
    else if (array.isInstanceOf[Array[Float]]) {
      val a = array.asInstanceOf[Array[Float]]
      for (i <- 0 to a.length-1) buffer.writeFloat(a(i))
    }
    else if (array.isInstanceOf[Array[Double]]) {
      val a = array.asInstanceOf[Array[Double]]
      for (i <- 0 to a.length-1) buffer.writeDouble(a(i))
    }
    else throw new IllegalArgumentException("BufferIO.writeArray does not support " + array.getClass)
  }


  final def readArray[@specialized A: ClassManifest](): Array[A] = {
    val length = readInt()
    val array = Array.ofDim[A](length)

    if (array.isInstanceOf[Array[Boolean]]) {
      val a = array.asInstanceOf[Array[Boolean]]
      for (i <- 0 to a.length-1) a(i) = readBoolean()
    }
    else if (array.isInstanceOf[Array[Byte]]) {
      val a = array.asInstanceOf[Array[Byte]]
      buffer.readBytes(a)
    }
    else if (array.isInstanceOf[Array[Short]]) {
      val a = array.asInstanceOf[Array[Short]]
      for (i <- 0 to a.length-1) a(i) = buffer.readShort()
    }
    else if (array.isInstanceOf[Array[Int]]) {
      val a = array.asInstanceOf[Array[Int]]
      for (i <- 0 to a.length-1) a(i) = buffer.readInt()
    }
    else if (array.isInstanceOf[Array[Long]]) {
      val a = array.asInstanceOf[Array[Long]]
      for (i <- 0 to a.length-1) a(i) = buffer.readLong()
    }
    else if (array.isInstanceOf[Array[Float]]) {
      val a = array.asInstanceOf[Array[Float]]
      for (i <- 0 to a.length-1) a(i) = buffer.readFloat()
    }
    else if (array.isInstanceOf[Array[Double]]) {
      val a = array.asInstanceOf[Array[Double]]
      for (i <- 0 to a.length-1) a(i) = buffer.readDouble()
    }
    else throw new IllegalArgumentException("BufferIO.readArray does not support " + array.getClass)

    array
  }
}


object BufferIO {
  def apply(): BufferIO = apply(1024)
  def apply(initialSize: Int): BufferIO = new BufferIO(NChannelBuffers.dynamicBuffer(initialSize))
  def apply(array: Array[Byte]): BufferIO = new BufferIO(NChannelBuffers.wrappedBuffer(array))
  def apply(buffer: NChannelBuffer): BufferIO = new BufferIO(buffer)
}
