// Copyright (C) 2010-2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.io.memory

import daqcore.util._


class MemRegion(val from: MemRegion.MemAddress, val until: MemRegion.MemAddress) {
  thisRegion =>

  import MemRegion._
  import Register._


  def parent: Option[MemRegion] = None

  class SubRegion(from: MemRegion.MemAddress, until: MemRegion.MemAddress)
    extends MemRegion(thisRegion.from + from, thisRegion.from+until)
  {
    override def parent: Option[MemRegion] = Some(thisRegion)
  }


  def length = until - from
  def size = length

  trait MemRegister[@specialized(Byte, Short, Int, Long) T] extends Register[T] {
    thisMemRegister =>

    case class FullValue(value: T) {
      final def register = thisMemRegister

      final def +|(x: PartialValue)(implicit numType: IntegerNumType[T]) =
        FullValue(numType.bitwiseMerge(value, x.value, x.bitMask))

      final def +|(bits: RegBitSelection, x: T)(implicit numType: IntegerNumType[T]) =
        FullValue(bits.setBits(value, x))

      final def +|(bit: RegSingleBit, x: Boolean)(implicit numType: IntegerNumType[T]) =
        FullValue(bit.setBit(value, x))

      override def toString = s"${getClass.getSimpleName}(${hex(value)})"
    }

    case class PartialValue(bmValue: BitMaskedInteger[T]) {
      final def register = thisMemRegister
      final def value = bmValue.value
      final def bitMask = bmValue.mask
      final def +|(that: PartialValue)(implicit numType: IntegerNumType[T]) = PartialValue(this.bmValue +| that.bmValue)
    }

    final def zero(implicit numType: IntegerNumType[T]) = FullValue(numType.zero)

    final def ~>(value: T) = FullValue(value)

    def region = thisRegion

    def address: MemAddress

    final def absoluteAddress: MemAddress = thisRegion.from + address

    trait MemBitSelection extends RegBitSelection {
      override def register: MemRegister[T] = thisMemRegister

      final def ~>(value: T)(implicit numType: IntegerNumType[T]): PartialValue =
        PartialValue(BitMaskedInteger(setBits(numType.zero, value), bitMask))
    }

    trait MemSingleBit extends RegSingleBit with MemBitSelection {
      final def ~>(value: Boolean)(implicit numType: IntegerNumType[T]): PartialValue =
        PartialValue(BitMaskedInteger(setBit(numType.zero, value), bitMask))
    }

    trait MemBitRange extends RegBitRange with MemBitSelection
  }



  trait ReadableRegister[@specialized(Byte, Short, Int, Long) T] extends MemRegister[T] { thisRegister =>
    trait ReadableBitSelection extends MemBitSelection
    trait ReadableBit extends MemSingleBit with ReadableBitSelection
    trait ReadableBitRange extends MemBitRange with ReadableBitSelection

    case class ROBit(n: Int) extends ReadableBit
    case class ROBitRange(bits: Range) extends ReadableBitRange

    class ReadableContent extends Content {
      type Fields = Seq[(String, ReadableBitSelection)]

      def fields: Fields = for {
        method <- thisRegister.getClass.getDeclaredMethods.toList
        if method.getParameterTypes.isEmpty
        if classOf[ReadableBitSelection].isAssignableFrom(method.getReturnType)
      } yield {
        method.getName -> method.invoke(thisRegister).asInstanceOf[ReadableBitSelection]
      }
    }
    def r = new ReadableContent
  }


  trait WriteableRegister[@specialized(Byte, Short, Int, Long) T] extends MemRegister[T] { thisRegister =>
    trait WriteableBitSelection extends MemBitSelection
    trait WriteableBit extends MemSingleBit with WriteableBitSelection
    trait WriteableBitRange extends MemBitRange with WriteableBitSelection

    case class WOBit(n: Int) extends WriteableBit
    case class WOBitRange(bits: Range) extends WriteableBitRange

    class WriteableContent extends Content {
      type Fields = Seq[(String, WriteableBitSelection)]

      def fields = for {
        method <- thisRegister.getClass.getDeclaredMethods.toList
        if method.getParameterTypes.isEmpty
        if classOf[WriteableBitSelection].isAssignableFrom(method.getReturnType)
      } yield {
        method.getName -> method.invoke(thisRegister).asInstanceOf[WriteableBitSelection]
      }
    }
    def w = new WriteableContent
  }


  sealed trait PartiallyWriteableRegister[@specialized(Byte, Short, Int, Long) T] extends WriteableRegister[T]


  class RORegister[@specialized(Byte, Short, Int, Long) T](val address: MemAddress) extends ReadableRegister[T]

  class WORegister[@specialized(Byte, Short, Int, Long) T](val address: MemAddress) extends WriteableRegister[T]

 
  class RWRegister[@specialized(Byte, Short, Int, Long) T](val address: MemAddress) extends ReadableRegister[T] with PartiallyWriteableRegister[T] {
    trait DirectRWBitSelection extends ReadableBitSelection with WriteableBitSelection

    case class RWBit(n: Int) extends ReadableBit with WriteableBit with DirectRWBitSelection
    case class RWBitRange(bits: Range) extends ReadableBitRange with WriteableBitRange with DirectRWBitSelection
  }


  class JKRegister[@specialized(Byte, Short, Int, Long) T](val address: MemAddress)(implicit val numType: IntegerNumType[T]) extends ReadableRegister[T] with PartiallyWriteableRegister[T] {
    trait JKBitSelection extends ReadableBitSelection with WriteableBitSelection

    case class JKBit(n: Int) extends ReadableBit with WriteableBit with JKBitSelection {
      require(n <= jkFirstClearBit, "Only lower half of bits can be declared in a J/K register")
    }

    case class JKBitRange(bits: Range) extends ReadableBitRange with WriteableBitRange with JKBitSelection {
      require(bits.end <= jkFirstClearBit, "Only lower half of bits can be declared in a J/K register")
    }
  }


  class KeyRegister[@specialized(Byte, Short, Int, Long) T](val address: MemAddress) extends WriteableRegister[T] {
    def writeValue(implicit numType: IntegerNumType[T]) = numType.one
  }
}


object MemRegion {
  type MemAddress = Long

  protected def jkFirstClearBit[T](implicit numType: IntegerNumType[T]) = (numType.nBits / 2)

  def jkValidBitMask[T](implicit numType: IntegerNumType[T]) = numType.invertBits(numType.minus(numType.shiftLeft(numType.one, jkFirstClearBit), numType.one))

  def isValidJKValue[T](value: T)(implicit numType: IntegerNumType[T]) = numType.bitwiseAnd(value, jkValidBitMask[T]) == numType.zero

  def jkWriteValue[T](value: T, bitMask: T)(implicit numType: IntegerNumType[T]) = {
    val maskedValue = numType.bitwiseAnd(value, bitMask)
    require(isValidJKValue(maskedValue), "Only lower half of bits is usable in a J/K register")
    numType.bitwiseOr(
      numType.bitwiseAnd(maskedValue, bitMask),
      numType.shiftLeft(numType.bitwiseAnd(numType.invertBits(maskedValue), bitMask), jkFirstClearBit)
    )
  }
}
