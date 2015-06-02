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

import scala.reflect.{ClassTag, classTag}

import daqcore.util._


trait BitCollection

trait Register[@specialized(Byte, Short, Int, Long) T] extends BitCollection { thisRegister =>
  import Register._

  trait RegBitSelection[@specialized(Int, Long, Float, Double)  U] extends BitSelection[T] {
    def register: Register[T] = thisRegister
    def conv(implicit numType: IntegerNumType[T]): ValueConv[T, U]
    def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): RegBitSelection[V]

    final def setConvBits(of: T, to: U)(implicit numType: IntegerNumType[T]): T = setBits(of, conv.applyRev(to))
    final def setConvBitsOpt(of: T, to: U)(implicit numType: IntegerNumType[T]): Option[T] = conv.unapply(to) map { x => setBits(of, x) }

    final def getConvBits(from: T)(implicit numType: IntegerNumType[T]): U = conv.apply(getBits(from))
  }


  trait RegSingleBit[@specialized(Int, Long, Float, Double)  U] extends Bit[T] with RegBitSelection[U]

  trait RegBitRange[@specialized(Int, Long, Float, Double)  U] extends BitRange[T] with RegBitSelection[U]


  trait BoolRegSingleBit extends RegSingleBit[Boolean] {
    def conv(implicit numType: IntegerNumType[T]) = Register.BitBoolConv[T]()
  }

  trait UnconvRegBitRange extends RegBitRange[T] {
    def conv(implicit numType: IntegerNumType[T]) = numType.identityConv
  }


  trait ConvValSingleBit[@specialized(Int, Long, Float, Double)  U] extends RegSingleBit[U] {
    def conversion: ValueConv[T, U]
    def conv(implicit numType: IntegerNumType[T]): ValueConv[T, U] = conversion
  }

  trait ConvValRegBitRange[@specialized(Int, Long, Float, Double)  U] extends RegBitRange[U] {
    def conversion: ValueConv[T, U]
    def conv(implicit numType: IntegerNumType[T]) = conversion
  }


  trait SubRegister extends BitCollection  // Not necessarily contiguous

  type Fields = Seq[(String, RegBitSelection[_])]

  abstract class Content {
    def fields: Fields
  }


  class AllContent extends Content {
    def fields: Fields = for {
      method <- getClass.getDeclaredMethods.toList
      if method.getParameterTypes.isEmpty
      if classOf[RegBitSelection[_]].isAssignableFrom(method.getReturnType)
    } yield {
      method.getName -> method.invoke(this).asInstanceOf[RegBitSelection[_]]
    }
  }
  def all = new AllContent

  final def nBytes(implicit numType: IntegerNumType[T]) = numType.nBytes
  final def nBits(implicit numType: IntegerNumType[T]) = numType.nBits
}


object Register {
  case class BitBoolConv[@specialized(Int, Long) Raw]()(implicit rawNT: IntegerNumType[Raw])
    extends ValueConv[Raw, Boolean] with ValueRevConv.SimpleUnapplyConv[Raw, Boolean]
  {
    def apply(raw: Raw) = {
      if (raw == rawNT.zero) false
      else if (raw == rawNT.one) true
      else throw new IllegalArgumentException(s"Raw value ${raw} can't be converted to bool, must be one or zero.")
    }

    def applyRev(conv: Boolean) = if (!conv) rawNT.zero else rawNT.one
  }
}


class SimpleRegister[@specialized(Byte, Short, Int, Long) T] extends Register[T] {
  thisRegister =>

  trait RegBitImpl[@specialized(Int, Long, Float, Double)  U] extends RegSingleBit[U] {
    def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): RegBitWithConv[V] =
      RegBitWithConv(n, conv andThen c)
  }

  case class RegBit(n: Int = 0) extends RegBitImpl[Boolean] with BoolRegSingleBit

  case class RegBitWithConv[@specialized(Int, Long, Float, Double)  U](n: Int = 0, conversion: ValueConv[T, U])
    extends RegBitImpl[U] with ConvValSingleBit[U]


  trait RegBitsImpl[@specialized(Int, Long, Float, Double)  U] extends RegBitRange[U] {
    def withConv[V](c: ValueConv[U, V])(implicit numType: IntegerNumType[T]): RegBitsWithConv[V] =
      RegBitsWithConv(bits, conv andThen c)
  }

  case class RegBits(bits: Range) extends RegBitsImpl[T] with UnconvRegBitRange

  case class RegBitsWithConv[@specialized(Int, Long, Float, Double)  U](bits: Range, conversion: ValueConv[T, U])
    extends RegBitsImpl[U] with ConvValRegBitRange[U]
}
