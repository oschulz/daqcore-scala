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

import java.util.UUID

import daqcore.util._


abstract class ContentSerializer[A <: Any : ClassManifest] {
  val mf = classManifest[A]
  val cl = mf.erasure

  def write(out: BasicOutput, x: A): Unit
  def read(in: BasicInput): A

  def writeObj(out: BasicOutput, x: Any) = write(out, x.asInstanceOf[A])
  def readObj(in: BasicInput) = read(in).asInstanceOf[AnyRef]
}


object ContentSerializer {
  def forType[A : ClassManifest]: ContentSerializer[A] = {
    val mf = classManifest[A]
    val cl = mf.erasure
    def unsupported = new IllegalArgumentException(getClass.toString + " does not support " + mf)

    val io = {
      if (mf == classManifest[Boolean]) BooleanIO
      else if (mf == classManifest[Byte]) ByteIO
      else if (mf == classManifest[Short]) ShortIO
      else if (mf == classManifest[Int]) IntIO
      else if (mf == classManifest[Long]) LongIO
      else if (mf == classManifest[Float]) FloatIO
      else if (mf == classManifest[Double]) DoubleIO
      else if (mf == classManifest[String]) StringIO
      else if (mf == classManifest[UUID]) UUIDIO
      /*else if (cl.isArray) {
        val cmf = ClassOps.manifestFromClass(cl.getComponentType)
        
        if (cmf == classManifest[Boolean]) BooleanArrayIO
        else if (cmf == classManifest[Byte]) ByteArrayIO
        else if (cmf == classManifest[Short]) ShortArrayIO
        else if (cmf == classManifest[Int]) IntArrayIO
        else if (cmf == classManifest[Long]) LongArrayIO
        else if (cmf == classManifest[Float]) FloatArrayIO
        else if (cmf == classManifest[Double]) DoubleArrayIO
        else throw unsupported
      }*/
      else if (classOf[Seq[_]].isAssignableFrom(cl)) {
        val cl = mf.erasure

        if (cl == classOf[ArrayVecBoolean]) ArrayVecBooleanIO
        else if (cl == classOf[ArrayVecByte]) ArrayVecByteIO
        else if (cl == classOf[ArrayVecShort]) ArrayVecShortIO
        else if (cl == classOf[ArrayVecInt]) ArrayVecIntIO
        else if (cl == classOf[ArrayVecLong]) ArrayVecLongIO
        else if (cl == classOf[ArrayVecFloat]) ArrayVecFloatIO
        else if (cl == classOf[ArrayVecDouble]) ArrayVecDoubleIO
        else throw unsupported
      }
      else if (classOf[Product].isAssignableFrom(mf.erasure)) ProductSerializer.forType[Product](mf.asInstanceOf[ClassManifest[Product]])
      else throw unsupported
    }
    io.asInstanceOf[ContentSerializer[A]]
  }

  object BooleanIO extends ContentSerializer[Boolean] {
    def write(out: BasicOutput, x: Boolean) = out.writeBoolean(x)
    def read(in: BasicInput) = in.readBoolean()
  }

  object ByteIO extends ContentSerializer[Byte] {
    def write(out: BasicOutput, x: Byte) = out.writeByte(x)
    def read(in: BasicInput) = in.readByte()
  }

  object ShortIO extends ContentSerializer[Short] {
    def write(out: BasicOutput, x: Short) = out.writeShort(x)
    def read(in: BasicInput) = in.readShort()
  }

  object IntIO extends ContentSerializer[Int] {
    def write(out: BasicOutput, x: Int) = out.writeInt(x)
    def read(in: BasicInput) = in.readInt()
  }

  object LongIO extends ContentSerializer[Long] {
    def write(out: BasicOutput, x: Long) = out.writeLong(x)
    def read(in: BasicInput) = in.readLong()
  }

  object FloatIO extends ContentSerializer[Float] {
    def write(out: BasicOutput, x: Float) = out.writeFloat(x)
    def read(in: BasicInput) = in.readFloat()
  }

  object DoubleIO extends ContentSerializer[Double] {
    def write(out: BasicOutput, x: Double) = out.writeDouble(x)
    def read(in: BasicInput) = in.readDouble()
  }

  object StringIO extends ContentSerializer[String] {
    def write(out: BasicOutput, x: String) = out.writeString(x)
    def read(in: BasicInput) = in.readString()
  }

  object UUIDIO extends ContentSerializer[UUID] {
    def write(out: BasicOutput, x: UUID) = out.writeUUID(x)
    def read(in: BasicInput) = in.readUUID()
  }

  class ArrayIO[A: ClassManifest] extends ContentSerializer[Array[A]] {
    def write(out: BasicOutput, x: Array[A]) = out.writeArray(x)
    def read(in: BasicInput) = in.readArray[A]()
  }

  object BooleanArrayIO extends ArrayIO[Boolean]
  object ByteArrayIO extends ArrayIO[Byte]
  object ShortArrayIO extends ArrayIO[Short]
  object IntArrayIO extends ArrayIO[Int]
  object LongArrayIO extends ArrayIO[Long]
  object FloatArrayIO extends ArrayIO[Float]
  object DoubleArrayIO extends ArrayIO[Double]

  class SeqIO[A: ClassManifest] extends ContentSerializer[Seq[A]] {
    def write(out: BasicOutput, x: Seq[A]) = out.writeSeq(x)
    def read(in: BasicInput) = in.readSeq[A]()
  }
  
  object BooleanSeqIO extends SeqIO[Boolean]
  object ByteSeqIO extends SeqIO[Byte]
  object ShortSeqIO extends SeqIO[Short]
  object IntSeqIO extends SeqIO[Int]
  object LongSeqIO extends SeqIO[Long]
  object FloatSeqIO extends SeqIO[Float]
  object DoubleSeqIO extends SeqIO[Double]
  
  object ArrayVecBooleanIO extends ContentSerializer[ArrayVecBoolean] {
    def write(out: BasicOutput, x: ArrayVecBoolean) = out.writeSeq(x)
    def read(in: BasicInput) = in.readSeq[Boolean]()
  }

  object ArrayVecByteIO extends ContentSerializer[ArrayVecByte] {
    def write(out: BasicOutput, x: ArrayVecByte) = out.writeSeq(x)
    def read(in: BasicInput) = in.readSeq[Byte]()
  }

  object ArrayVecShortIO extends ContentSerializer[ArrayVecShort] {
    def write(out: BasicOutput, x: ArrayVecShort) = out.writeSeq(x)
    def read(in: BasicInput) = in.readSeq[Short]()
  }

  object ArrayVecIntIO extends ContentSerializer[ArrayVecInt] {
    def write(out: BasicOutput, x: ArrayVecInt) = out.writeSeq(x)
    def read(in: BasicInput) = in.readSeq[Int]()
  }

  object ArrayVecLongIO extends ContentSerializer[ArrayVecLong] {
    def write(out: BasicOutput, x: ArrayVecLong) = out.writeSeq(x)
    def read(in: BasicInput) = in.readSeq[Long]()
  }

  object ArrayVecFloatIO extends ContentSerializer[ArrayVecFloat] {
    def write(out: BasicOutput, x: ArrayVecFloat) = out.writeSeq(x)
    def read(in: BasicInput) = in.readSeq[Float]()
  }

  object ArrayVecDoubleIO extends ContentSerializer[ArrayVecDouble] {
    def write(out: BasicOutput, x: ArrayVecDouble) = out.writeSeq(x)
    def read(in: BasicInput) = in.readSeq[Double]()
  }
}
