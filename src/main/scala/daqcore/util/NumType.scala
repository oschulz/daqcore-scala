// Copyright (C) 2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR T PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received x copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


package daqcore.util


trait NumType[@specialized(Byte, Short, Int, Long, Float, Double) T] {
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def times(x: T, y: T): T
  def divide(x: T, y: T): T

  def zero: T
  def one: T

  final def from(x: Byte): T = fromByte(x)
  final def from(x: Short): T = fromShort(x)
  final def from(x: Int): T = fromInt(x)
  final def from(x: Long): T = fromLong(x)

  def fromByte(x: Byte): T
  def fromShort(x: Short): T
  def fromInt(x: Int): T
  def fromLong(x: Long): T
  def fromDouble(x: Double): T
  def fromFloat(x: Float): T

  def toByte(x: T): Byte
  def toShort(x: T): Short
  def toInt(x: T): Int
  def toLong(x: T): Long
  def toFloat(x: T): Float
  def toDouble(x: T): Double
}


trait IntegerNumType[@specialized(Byte, Short, Int, Long) T] extends NumType[T] {
  def unsignedMax: T

  def modulo(x: T, y: T): T

  def nBytes: Int
  def nBits: Int

  def invertBits(x: T): T
  def bitwiseAnd(x: T, y: T): T
  def bitwiseOr(x: T, y: T): T

  def shiftLeft(x: T, n: Int): T
  def signedShiftRight(x: T, n: Int): T
  def unsignedShiftRight(x: T, n: Int): T

  def numberOfLeadingZeros(x: T): Int
  def numberOfTrailingZeros(x: T): Int

  final def validBitNo(bit: Int): Boolean = (bit >= 0) && (bit < nBits)
  final def bitMask(bit: Int): T = shiftLeft(one, bit)
  final def nBitsMask(nBits: Int): T = if (nBits >= this.nBits) minus(zero, one) else minus(shiftLeft(one, nBits), one)
  final def bitMask(firstBit: Int, nBits: Int): T = shiftLeft(nBitsMask(nBits), firstBit)

  final def bitMask(bits: Range): T = {
    require(bits.step == 1, "Range with step size 1 required")
    bitMask(bits.start, bits.size)
  }

  final def invBitMask(bit: Int): T = invertBits(bitMask(bit))
  final def invNBitsMask(nBits: Int): T = invertBits(nBitsMask(nBits))
  final def invBitMask(firstBit: Int, nBits: Int): T = invertBits(bitMask(firstBit, nBits))

  final def bitwiseMerge(x: T, y: T, mask: T) = {
    bitwiseOr( bitwiseAnd(x, invertBits(mask)), bitwiseAnd(y, mask) )
  }

  final def getBit(bit: Int, from: T): Boolean = {
    if (validBitNo(bit)) bitwiseAnd(from, bitMask(bit)) != zero
    else false
  }

  final def setBit(bit: Int, of: T, to: Boolean): T = if (to) setBit(bit, of) else clearBit(bit, of)

  final def setBit(bit: Int, of: T): T = {
    if (validBitNo(bit)) bitwiseOr(of, bitMask(bit))
    else of
  }

  final def clearBit(bit: Int, of: T): T = {
    if (validBitNo(bit)) bitwiseAnd(of, invBitMask(bit))
    else of
  }

  final def getBitRange(firstBit: Int, nBits: Int, from: T): T = {
    if (validBitNo(firstBit)) unsignedShiftRight(bitwiseAnd(from, bitMask(firstBit, nBits)), firstBit)
    else zero
  }


  final def setBitRange(firstBit: Int, nBits: Int, of: T, to: T): T = {
    if (validBitNo(firstBit)) {
      val maskedOf = bitwiseAnd(of, invBitMask(firstBit, nBits))
      val shiftedTo = bitwiseAnd(shiftLeft(to, firstBit), bitMask(firstBit, nBits))
      bitwiseOr(maskedOf, shiftedTo)
    } else {
      of
    }
  }

  final def getBits(bits: Range, from: T): T = {
    require(bits.step == 1, "Range with step size 1 required")
    getBitRange(bits.start, bits.size, from)
  }

  final def setBits(bits: Range, of: T, to: T): T = {
    require(bits.step == 1, "Range with step size 1 required")
    setBitRange(bits.start, bits.size, of, to)
  }
}


object IntegerNumType {

  final class IntegerNumTypeOps[T](val x: T) extends AnyVal {
    def +(y: T)(implicit numType: IntegerNumType[T]) = numType.plus(x, y)
    def -(y: T)(implicit numType: IntegerNumType[T]) = numType.minus(x, y)
    def *(y: T)(implicit numType: IntegerNumType[T]) = numType.times(x, y)
    def /(y: T)(implicit numType: IntegerNumType[T]) = numType.divide(x, y)
    def %(y: T)(implicit numType: IntegerNumType[T]) = numType.modulo(x, y)

    def ~(implicit numType: IntegerNumType[T]) = numType.invertBits(x)
    def &(y: T)(implicit numType: IntegerNumType[T]) = numType.bitwiseAnd(x, y)
    def |(y: T)(implicit numType: IntegerNumType[T]) = numType.bitwiseOr(x, y)

    def <<(n: Int)(implicit numType: IntegerNumType[T]) = numType.shiftLeft(x, n)
    def >>(n: Int)(implicit numType: IntegerNumType[T]) = numType.signedShiftRight(x, n)
    def >>>(n: Int)(implicit numType: IntegerNumType[T]) = numType.unsignedShiftRight(x, n)

    def numberOfLeadingZeros(implicit numType: IntegerNumType[T]) = numType.numberOfLeadingZeros(x)  
    def numberOfTrailingZeros(implicit numType: IntegerNumType[T]) = numType.numberOfTrailingZeros(x)  

    def bitwiseMerge(y: T, mask: T)(implicit numType: IntegerNumType[T]) = numType.bitwiseMerge(x, y, mask)

    def toByte(x: T)(implicit numType: IntegerNumType[T]) = numType.toByte(x)
    def toShort(x: T)(implicit numType: IntegerNumType[T]) = numType.toShort(x)
    def toInt(x: T)(implicit numType: IntegerNumType[T]) = numType.toInt(x)
    def toLong(x: T)(implicit numType: IntegerNumType[T]) = numType.toLong(x)
    def toFloat(x: T)(implicit numType: IntegerNumType[T]) = numType.toFloat(x)
    def toDouble(x: T)(implicit numType: IntegerNumType[T]) = numType.toDouble(x)

    def getBit(bit: Int)(implicit numType: IntegerNumType[T]) = numType.getBit(bit, x)

    def setBit(bit: Int, to: Boolean)(implicit numType: IntegerNumType[T])  = numType.setBit(bit, x, to)

    def setBit(bit: Int)(implicit numType: IntegerNumType[T]) = numType.setBit(bit, x)
    def clearBit(bit: Int)(implicit numType: IntegerNumType[T]) = numType.clearBit(bit, x)

    def getBitRange(firstBit: Int, nBits: Int)(implicit numType: IntegerNumType[T]) = numType.getBitRange(firstBit, nBits, x)
    def setBitRange(firstBit: Int, nBits: Int, to: T)(implicit numType: IntegerNumType[T]) = numType.setBitRange(firstBit, nBits, x, to)

    def getBits(bits: Range)(implicit numType: IntegerNumType[T]) = numType.getBits(bits, x)
    def setBits(bits: Range, to: T)(implicit numType: IntegerNumType[T]) = numType.setBits(bits, x, to)
  }


  implicit object ByteNumType extends IntegerNumType[Byte] {
    def plus(x: Byte, y: Byte) = (x + y).toByte
    def minus(x: Byte, y: Byte) = (x - y).toByte
    def times(x: Byte, y: Byte) = (x * y).toByte
    def divide(x: Byte, y: Byte) = (x / y).toByte

    def zero = 0.toByte
    def one = 1.toByte
    def unsignedMax = -1.toByte

    def fromByte(x: Byte) = x
    def fromShort(x: Short) = x.toByte
    def fromInt(x: Int) = x.toByte
    def fromLong(x: Long) = x.toByte
    def fromDouble(x: Double) = x.toByte
    def fromFloat(x: Float) = x.toByte

    def toByte(x: Byte) = x
    def toShort(x: Byte) = x.toShort
    def toInt(x: Byte) = x.toInt
    def toLong(x: Byte) = x.toLong
    def toFloat(x: Byte) = x.toFloat
    def toDouble(x: Byte) = x.toDouble

    def modulo(x: Byte, y: Byte) = (x % y).toByte

    def nBytes = 1
    def nBits = 8 * nBytes

    def invertBits(x: Byte) = (~x).toByte
    def bitwiseAnd(x: Byte, y: Byte) = (x & y).toByte
    def bitwiseOr(x: Byte, y: Byte) = (x | y).toByte

    def shiftLeft(x: Byte, n: Int) = (x << n).toByte
    def signedShiftRight(x: Byte, n: Int) = (x >> n).toByte
    def unsignedShiftRight(x: Byte, n: Int) = (x.asUnsigned >>> n).toByte

    def numberOfLeadingZeros(x: Byte) = {
      if (x != zero) IntNumType.numberOfLeadingZeros(x.asUnsigned)
      else nBits
    }

    def numberOfTrailingZeros(x: Byte) = {
      if (x != zero) IntNumType.numberOfTrailingZeros(x.asUnsigned)
      else nBits
    }
  }


  implicit object ShortNumType extends IntegerNumType[Short] {
    def plus(x: Short, y: Short) = (x + y).toShort
    def minus(x: Short, y: Short) = (x - y).toShort
    def times(x: Short, y: Short) = (x * y).toShort
    def divide(x: Short, y: Short) = (x / y).toShort

    def zero = 0.toShort
    def one = 1.toShort
    def unsignedMax = -1.toShort

    def fromByte(x: Byte) = x.toShort
    def fromShort(x: Short) = x
    def fromInt(x: Int) = x.toShort
    def fromLong(x: Long) = x.toShort
    def fromDouble(x: Double) = x.toShort
    def fromFloat(x: Float) = x.toShort

    def toByte(x: Short) = x.toByte
    def toShort(x: Short) = x
    def toInt(x: Short) = x.toInt
    def toLong(x: Short) = x.toLong
    def toFloat(x: Short) = x.toFloat
    def toDouble(x: Short) = x.toDouble

    def modulo(x: Short, y: Short) = (x % y).toShort

    def nBytes = 2
    def nBits = 8 * nBytes
  
    def invertBits(x: Short) = (~x).toShort
    def bitwiseAnd(x: Short, y: Short) = (x & y).toShort
    def bitwiseOr(x: Short, y: Short) = (x | y).toShort

    def shiftLeft(x: Short, n: Int) = (x << n).toShort
    def signedShiftRight(x: Short, n: Int) = (x >> n).toShort
    def unsignedShiftRight(x: Short, n: Int) = (x.asUnsigned >>> n).toShort

    def numberOfLeadingZeros(x: Short) = {
      if (x != zero) IntNumType.numberOfLeadingZeros(x.asUnsigned)
      else nBits
    }

    def numberOfTrailingZeros(x: Short) = {
      if (x != zero) IntNumType.numberOfTrailingZeros(x.asUnsigned)
      else nBits
    }
  }


  implicit object IntNumType extends IntegerNumType[Int] {
    def plus(x: Int, y: Int) = x + y
    def minus(x: Int, y: Int) = x - y
    def times(x: Int, y: Int) = x * y
    def divide(x: Int, y: Int) = x / y

    def zero = 0
    def one = 1
    def unsignedMax = -1

    def fromByte(x: Byte) = x.toInt
    def fromShort(x: Short) = x.toInt
    def fromInt(x: Int) = x
    def fromLong(x: Long) = x.toInt
    def fromDouble(x: Double) = x.toInt
    def fromFloat(x: Float) = x.toInt

    def toByte(x: Int) = x.toByte
    def toShort(x: Int) = x.toShort
    def toInt(x: Int) = x
    def toLong(x: Int) = x.toLong
    def toFloat(x: Int) = x.toFloat
    def toDouble(x: Int) = x.toDouble

    def modulo(x: Int, y: Int) = x % y

    def nBytes = 4
    def nBits = 8 * nBytes

    def invertBits(x: Int) = ~x
    def bitwiseAnd(x: Int, y: Int) = x & y
    def bitwiseOr(x: Int, y: Int) = x | y

    def shiftLeft(x: Int, n: Int) = x << n
    def signedShiftRight(x: Int, n: Int) = x >> n
    def unsignedShiftRight(x: Int, n: Int) = x >>> n

    def numberOfLeadingZeros(x: Int) = Integer.numberOfLeadingZeros(x)

    def numberOfTrailingZeros(x: Int) = Integer.numberOfTrailingZeros(x)
  }


  implicit object LongNumType extends IntegerNumType[Long] {
    def plus(x: Long, y: Long) = x + y
    def minus(x: Long, y: Long) = x - y
    def times(x: Long, y: Long) = x * y
    def divide(x: Long, y: Long) = x / y

    def zero = 0L
    def one = 1L
    def unsignedMax = -1L

    def fromByte(x: Byte) = x.toLong
    def fromShort(x: Short) = x.toLong
    def fromInt(x: Int) = x.toLong
    def fromLong(x: Long) = x
    def fromDouble(x: Double) = x.toLong
    def fromFloat(x: Float) = x.toLong

    def toByte(x: Long) = x.toByte
    def toShort(x: Long) = x.toShort
    def toInt(x: Long) = x.toInt
    def toLong(x: Long) = x
    def toFloat(x: Long) = x.toFloat
    def toDouble(x: Long) = x.toDouble

    def modulo(x: Long, y: Long) = x % y

    def nBytes = 8
    def nBits = 8 * nBytes

    def invertBits(x: Long) = ~x
    def bitwiseAnd(x: Long, y: Long) = x & y
    def bitwiseOr(x: Long, y: Long) = x | y

    def shiftLeft(x: Long, n: Int) = x << n
    def signedShiftRight(x: Long, n: Int) = x >> n
    def unsignedShiftRight(x: Long, n: Int) = x >>> n

    def numberOfLeadingZeros(x: Long) = java.lang.Long.numberOfLeadingZeros(x)

    def numberOfTrailingZeros(x: Long) = java.lang.Long.numberOfTrailingZeros(x)
  }

}
