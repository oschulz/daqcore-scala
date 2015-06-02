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

import daqcore.io._
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

      final def +/|(bits: RegBitSelection[_], x: T)(implicit numType: IntegerNumType[T]) =
        FullValue(bits.setBits(value, x))

      final def +|[U](bits: RegBitSelection[U], x: U)(implicit numType: IntegerNumType[T]) =
        FullValue(bits.setBits(value, bits.conv.applyRev(x)))

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

    trait MemBitSelection[@specialized(Int, Long, Float, Double)  U] extends RegBitSelection[U] {
      override def register: MemRegister[T] = thisMemRegister

      final def ~/>(value: T)(implicit numType: IntegerNumType[T]): PartialValue =
        PartialValue(BitMaskedInteger(setBits(numType.zero, value), bitMask))

      final def ~>(value: U)(implicit numType: IntegerNumType[T]): PartialValue =
        PartialValue(BitMaskedInteger(setBits(numType.zero, conv.applyRev(value)), bitMask))
    }

    trait MemSingleBit[@specialized(Int, Long, Float, Double)  U] extends RegSingleBit[U] with MemBitSelection[U]

    trait MemBitRange[@specialized(Int, Long, Float, Double)  U] extends RegBitRange[U] with MemBitSelection[U]
  }



  trait ReadableRegister[@specialized(Byte, Short, Int, Long) T] extends MemRegister[T] { thisRegister =>
    trait ReadableBitSelection[@specialized(Int, Long, Float, Double)  U] extends MemBitSelection[U]
    trait ReadableBit[@specialized(Int, Long, Float, Double)  U] extends MemSingleBit[U] with ReadableBitSelection[U]
    trait ReadableBitRange[@specialized(Int, Long, Float, Double)  U] extends MemBitRange[U] with ReadableBitSelection[U]


    trait ROBitImpl[@specialized(Int, Long, Float, Double)  U] extends ReadableBit[U] {
      def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): ReadableBit[V] =
        ROBitWithConv(n, conv andThen c)
    }

    case class ROBit(n: Int = 0) extends ROBitImpl[Boolean] with BoolRegSingleBit

    case class ROBitWithConv[@specialized(Int, Long, Float, Double)  U](n: Int = 0, conversion: ValueConv[T, U])
      extends ROBitImpl[U] with ConvValSingleBit[U]


    trait ReadableBitRangeImpl[@specialized(Int, Long, Float, Double)  U] extends ReadableBitRange[U] {
      def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): ReadableBitRange[V] =
        ROBitsWithConv(bits, conv andThen c)
    }

    case class ROBits(bits: Range) extends ReadableBitRangeImpl[T] with UnconvRegBitRange

    case class ROBitsWithConv[@specialized(Int, Long, Float, Double)  U](bits: Range, conversion: ValueConv[T, U])
      extends ReadableBitRangeImpl[U] with ConvValRegBitRange[U]


    class ReadableContent extends Content {
      type Fields = Seq[(String, ReadableBitSelection[_])]

      def fields: Fields = for {
        method <- thisRegister.getClass.getDeclaredMethods.toList
        if method.getParameterTypes.isEmpty
        if classOf[ReadableBitSelection[_]].isAssignableFrom(method.getReturnType)
      } yield {
        method.getName -> method.invoke(thisRegister).asInstanceOf[ReadableBitSelection[_]]
      }
    }
    def r = new ReadableContent
  }


  trait WriteableRegister[@specialized(Byte, Short, Int, Long) T] extends MemRegister[T] { thisRegister =>
    trait WriteableBitSelection[@specialized(Int, Long, Float, Double)  U] extends MemBitSelection[U]
    trait WriteableSingleBit[@specialized(Int, Long, Float, Double)  U] extends MemSingleBit[U] with WriteableBitSelection[U]
    trait WriteableBitRange[@specialized(Int, Long, Float, Double)  U] extends MemBitRange[U] with WriteableBitSelection[U]


    trait WOBitImpl[@specialized(Int, Long, Float, Double)  U] extends WriteableSingleBit[U] {
      def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): WriteableSingleBit[V] =
        WOBitWithConv(n, conv andThen c)
    }

    case class WOBit(n: Int = 0) extends WOBitImpl[Boolean] with BoolRegSingleBit

    case class WOBitWithConv[@specialized(Int, Long, Float, Double)  U](n: Int = 0, conversion: ValueConv[T, U])
      extends WOBitImpl[U] with ConvValSingleBit[U]


    trait WriteableBitRangeImpl[@specialized(Int, Long, Float, Double)  U] extends WriteableBitRange[U] {
      def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): WriteableBitRange[V] =
        WOBitsWithConv(bits, conv andThen c)
    }

    case class WOBits(bits: Range) extends WriteableBitRangeImpl[T] with UnconvRegBitRange

    case class WOBitsWithConv[@specialized(Int, Long, Float, Double)  U](bits: Range, conversion: ValueConv[T, U])
      extends WriteableBitRangeImpl[U] with ConvValRegBitRange[U]


    class WriteableContent extends Content {
      type Fields = Seq[(String, WriteableBitSelection[_])]

      def fields = for {
        method <- thisRegister.getClass.getDeclaredMethods.toList
        if method.getParameterTypes.isEmpty
        if classOf[WriteableBitSelection[_]].isAssignableFrom(method.getReturnType)
      } yield {
        method.getName -> method.invoke(thisRegister).asInstanceOf[WriteableBitSelection[_]]
      }
    }
    def w = new WriteableContent
  }


  trait GenericRWRegister[@specialized(Byte, Short, Int, Long) T] extends ReadableRegister[T] with WriteableRegister[T] {
    trait RWBitSelection[@specialized(Int, Long, Float, Double)  U] extends ReadableBitSelection[U] with WriteableBitSelection[U]
    trait RWSingleBit[@specialized(Int, Long, Float, Double)  U] extends ReadableBit[U] with WriteableSingleBit[U] with RWBitSelection[U]
    trait RWBitRange[@specialized(Int, Long, Float, Double)  U] extends ReadableBitRange[U] with WriteableBitRange[U] with RWBitSelection[U]
  }


  sealed trait PartiallyWriteableRegister[@specialized(Byte, Short, Int, Long) T] extends WriteableRegister[T]


  class RORegister[@specialized(Byte, Short, Int, Long) T](val address: MemAddress) extends ReadableRegister[T]

  class WORegister[@specialized(Byte, Short, Int, Long) T](val address: MemAddress) extends WriteableRegister[T]

 
  // Replace-Value Register, partial update via read/write makes no sense
  class RVRegister[@specialized(Byte, Short, Int, Long) T](val address: MemAddress) extends GenericRWRegister[T] {
    trait RWBitImpl[@specialized(Int, Long, Float, Double)  U] extends RWSingleBit[U] {
      def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): RWSingleBit[V] =
        RWBitWithConv(n, conv andThen c)
    }

    case class RWBit(n: Int = 0) extends RWBitImpl[Boolean] with BoolRegSingleBit

    case class RWBitWithConv[@specialized(Int, Long, Float, Double)  U](n: Int = 0, conversion: ValueConv[T, U])
      extends RWBitImpl[U] with ConvValSingleBit[U]


    trait RWBitRangeImpl[@specialized(Int, Long, Float, Double)  U] extends RWBitRange[U] {
      def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): RWBitRange[V] =
        RWBitsWithConv(bits, conv andThen c)
    }

    case class RWBits(bits: Range) extends RWBitRangeImpl[T] with UnconvRegBitRange

    case class RWBitsWithConv[@specialized(Int, Long, Float, Double)  U](bits: Range, conversion: ValueConv[T, U])
      extends RWBitRangeImpl[U] with ConvValRegBitRange[U]
  }


  class RWRegister[@specialized(Byte, Short, Int, Long) T](val address: MemAddress) extends GenericRWRegister[T] with PartiallyWriteableRegister[T] {
    trait DirectRWBitSelection[@specialized(Int, Long, Float, Double)  U] extends RWBitSelection[U]
    trait DirectRWSingleBit[@specialized(Int, Long, Float, Double)  U] extends RWSingleBit[U] with DirectRWBitSelection[U]
    trait DirectRWBitRange[@specialized(Int, Long, Float, Double)  U] extends RWBitRange[U] with DirectRWBitSelection[U]

    trait RWBitImpl[@specialized(Int, Long, Float, Double)  U] extends DirectRWSingleBit[U] {
      def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): DirectRWSingleBit[V] =
        RWBitWithConv(n, conv andThen c)
    }

    case class RWBit(n: Int = 0) extends RWBitImpl[Boolean] with BoolRegSingleBit

    case class RWBitWithConv[@specialized(Int, Long, Float, Double)  U](n: Int = 0, conversion: ValueConv[T, U])
      extends RWBitImpl[U] with ConvValSingleBit[U]


    trait RWBitRangeImpl[@specialized(Int, Long, Float, Double)  U] extends DirectRWBitRange[U] {
      def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): DirectRWBitRange[V] =
        RWBitsWithConv(bits, conv andThen c)
    }

    case class RWBits(bits: Range) extends RWBitRangeImpl[T] with UnconvRegBitRange

    case class RWBitsWithConv[@specialized(Int, Long, Float, Double)  U](bits: Range, conversion: ValueConv[T, U])
      extends RWBitRangeImpl[U] with ConvValRegBitRange[U]
  }


  class JKRegister[@specialized(Byte, Short, Int, Long) T](val address: MemAddress)(implicit val numType: IntegerNumType[T]) extends ReadableRegister[T] with PartiallyWriteableRegister[T] {
    trait JKBitSelection[@specialized(Int, Long, Float, Double)  U] extends ReadableBitSelection[U] with WriteableBitSelection[U]
    trait JKSingleBit[@specialized(Int, Long, Float, Double)  U] extends ReadableBit[U] with WriteableSingleBit[U] with JKBitSelection[U]
    trait JKBitRange[@specialized(Int, Long, Float, Double)  U] extends ReadableBitRange[U] with WriteableBitRange[U] with JKBitSelection[U]

    trait JKBitImpl[@specialized(Int, Long, Float, Double)  U] extends JKSingleBit[U] {
      require(n <= jkFirstClearBit, "Only lower half of bits can be declared in a J/K register")

      def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): JKSingleBit[V] =
        JKBitWithConv(n, conv andThen c)
    }

    case class JKBit(n: Int = 0) extends JKBitImpl[Boolean] with BoolRegSingleBit

    case class JKBitWithConv[@specialized(Int, Long, Float, Double)  U](n: Int = 0, conversion: ValueConv[T, U])
      extends JKBitImpl[U] with ConvValSingleBit[U]


    trait JKBitRangeImpl[@specialized(Int, Long, Float, Double)  U] extends JKBitRange[U] {
      require(bits.end <= jkFirstClearBit, "Only lower half of bits can be declared in a J/K register")

      def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): JKBitRange[V] =
        JKBitsWithConv(bits, conv andThen c)
    }

    case class JKBits(bits: Range) extends JKBitRangeImpl[T] with UnconvRegBitRange

    case class JKBitsWithConv[@specialized(Int, Long, Float, Double)  U](bits: Range, conversion: ValueConv[T, U])
      extends JKBitRangeImpl[U] with ConvValRegBitRange[U]
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
