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

  def readArray[@specialized A: ClassManifest](): Array[A] =
    readSeq[A]().toArray
  
  def readSeq[A: ClassManifest](): ArrayVec[A]
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

  def writeArray[@specialized A: ClassManifest](array: Array[A]): Unit =
    writeSeq(ArrayVec.wrap(array))
  
  def writeSeq[A: ClassManifest](seq: Seq[A]): Unit
}


case class RootEncInput(val source: ByteSeqIterator) extends BasicInput {
  implicit val enc = BigEndian

  final def readBoolean() = (enc.getByte(source) > 0)
  final def readByte() = enc.getByte(source)
  final def readShort() = enc.getShort(source)
  final def readInt() = enc.getInt(source)
  final def readLong() = enc.getLong(source)
  final def readFloat() = enc.getFloat(source)
  final def readDouble() = enc.getDouble(source)

  final def readString() = {
    val length = {
      val l = readByte()
      if (l < 0xff) l else readInt()
    }
    val a = Array.ofDim[Byte](length)
    require(source.len >= length)
    source.copyToArray(a)
    new String(a)
  }

  final def readUUID() = {
    val mostSigBits = readLong()
    val leastSigBits = readLong()
    new UUID(mostSigBits, leastSigBits)
  }

  final def readSeq[A: ClassManifest](): ArrayVec[A] = {
    val mf = classManifest[A]
    val length = readInt()

    if (mf == classManifest[Boolean]) enc.getBytes(source, length).map{_ > 0}.asInstanceOf[ArrayVec[A]]
    else if (mf == classManifest[Byte]) enc.getBytes(source, length).asInstanceOf[ArrayVec[A]]
    else if (mf == classManifest[Short]) enc.getShorts(source, length).asInstanceOf[ArrayVec[A]]
    else if (mf == classManifest[Int]) enc.getInts(source, length).asInstanceOf[ArrayVec[A]]
    else if (mf == classManifest[Long]) enc.getLongs(source, length).asInstanceOf[ArrayVec[A]]
    else if (mf == classManifest[Float]) enc.getFloats(source, length).asInstanceOf[ArrayVec[A]]
    else if (mf == classManifest[Double]) enc.getDoubles(source, length).asInstanceOf[ArrayVec[A]]
    else throw new UnsupportedOperationException("RootEncInput.readSeq() does not support " + mf)
  }
}


case class RootEncOutput(val target: ByteSeqBuilder) extends BasicOutput {
  implicit val enc = BigEndian

  def clear() = target.clear()

  def result() = target.result()

  final def writeBoolean(x: Boolean) = enc.putByte(target, if (x) 1 else 0)
  final def writeByte(x: Byte) = enc.putByte(target, x)
  final def writeShort(x: Short) = enc.putShort(target, x)
  final def writeInt(x: Int) = enc.putInt(target, x)
  final def writeLong(x: Long) = enc.putLong(target, x)
  final def writeFloat(x: Float) = enc.putFloat(target, x)
  final def writeDouble(x: Double) = enc.putDouble(target, x)

  final def writeString(x: String) = {
    if (x.length < 0xff) writeByte(x.length.toByte)
    else { writeByte(0xff.toByte); writeInt(x.length) }
    target ++= x.getBytes()
  }

  final def writeUUID(x: UUID) = {
    val mostSigBits = x.getMostSignificantBits
    val leastSigBits = x.getLeastSignificantBits
    writeLong(mostSigBits)
    writeLong(leastSigBits)
  }

  def writeSeq[A: ClassManifest](seq: Seq[A]) = {
    val mf = classManifest[A]
    writeInt(seq.length)

    val xs = seq.toArrayVec
    if (mf == classManifest[Boolean]) enc.putBytes(target, xs.asInstanceOf[ArrayVec[Boolean]].map{x => (if (x) 1 else 0).toByte})
    else if (mf == classManifest[Byte]) enc.putBytes(target, xs.asInstanceOf[ArrayVec[Byte]])
    else if (mf == classManifest[Short]) enc.putShorts(target, xs.asInstanceOf[ArrayVec[Short]])
    else if (mf == classManifest[Int]) enc.putInts(target, xs.asInstanceOf[ArrayVec[Int]])
    else if (mf == classManifest[Long]) enc.putLongs(target, xs.asInstanceOf[ArrayVec[Long]])
    else if (mf == classManifest[Float]) enc.putFloats(target, xs.asInstanceOf[ArrayVec[Float]])
    else if (mf == classManifest[Double]) enc.putDoubles(target, xs.asInstanceOf[ArrayVec[Double]])
    else throw new UnsupportedOperationException("RootEncOutput.writeSeq(...) does not support " + mf)
  }
}
