// Copyright (C) 2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.language.implicitConversions
import scala.collection.breakOut


trait ValueFwdConv[
  @specialized(Int, Long, Float, Double) -Raw,
  @specialized(Int, Long, Float, Double) +Conv
] extends Function1[Raw, Conv] {
  def apply(raw: Raw): Conv
}



trait ValueRevConv[
  @specialized(Int, Long, Float, Double) +Raw,
  @specialized(Int, Long, Float, Double) -Conv
] {
  def applyRev(conv: Conv): Raw
  def unapply(conv: Conv): Option[Raw]
}


object ValueRevConv {
  trait SimpleUnapplyConv[
    @specialized(Int, Long, Float, Double) +Raw,
    @specialized(Int, Long, Float, Double) -Conv
  ] extends ValueRevConv[Raw, Conv] {
    def unapply(conv: Conv) = Some(applyRev(conv))
  }


  trait ExceptionToNoneUnapplyConv[
    @specialized(Int, Long, Float, Double) +Raw,
    @specialized(Int, Long, Float, Double) -Conv
  ] extends ValueRevConv[Raw, Conv] {
    def unapply(conv: Conv) = try {
      Some(applyRev(conv))
    } catch {
      case _: IllegalArgumentException => None
    }    
  }
}



trait ValueConv[
  @specialized(Int, Long, Float, Double) Raw,
  @specialized(Int, Long, Float, Double) Conv
] extends ValueFwdConv[Raw, Conv] with ValueRevConv[Raw, Conv] {
  def andThen[Conv2](that: ValueConv[Conv, Conv2]): ValueConv[Raw, Conv2] = ValueConv.MappedConv(this, that)
}


object ValueConv {
  case class IdentityConv[@specialized(Int, Long, Float, Double) T]()
    extends ValueConv[T, T] with ValueRevConv.SimpleUnapplyConv[T, T]
  {
    def apply(raw: T) = raw
    def applyRev(conv: T) = conv
    override def andThen[Conv2](that: ValueConv[T, Conv2]) = that
  }

  def identity[T](): IdentityConv[T] = IdentityConv[T]()
  def apply[T](): IdentityConv[T] = IdentityConv[T]()



  case class FwdRevConv[Raw, Conv](raw2conv: PartialFunction[Raw, Conv])(conv2raw: PartialFunction[Conv, Raw]) {
    protected val liftedConv2raw = conv2raw.lift
    def apply(raw: Raw) = raw2conv
    def applyRev(conv: Conv) = conv2raw
    def unapply(conv: Conv) = liftedConv2raw
  }

  def apply[Raw, Conv](raw2conv: PartialFunction[Raw, Conv])(conv2raw: PartialFunction[Conv, Raw]) =
    FwdRevConv(raw2conv)(conv2raw)


  case class MappedConv[A, B, C](conv1: ValueConv[A, B], conv2: ValueConv[B, C]) extends ValueConv[A, C] {
    def apply(raw: A) = conv2(conv1(raw))
    def applyRev(conv: C) = conv1.applyRev(conv2.applyRev(conv))
    def unapply(conv: C) = conv2.unapply(conv) flatMap conv1.unapply
  }


  case class EnumConv(enum: Enumeration) extends ValueConv[Int, Enumeration#Value] {
    def apply(raw: Int) = enum(raw)

    def applyRev(conv: Enumeration#Value) = conv.id
    def unapply(conv: Enumeration#Value) = Some(conv.id)
  }

  implicit def apply(enum: Enumeration): EnumConv = EnumConv(enum)



  case class MapConv[Raw, Conv](raw2conv: Map[Raw, Conv]) extends ValueConv[Raw, Conv] {
    val conv2Raw: Map[Conv, Raw] = raw2conv.map{ case(k, v) => (v, k) }(breakOut)

    def apply(raw: Raw) = raw2conv(raw)

    def applyRev(conv: Conv) = conv2Raw(conv)
    def unapply(conv: Conv) = conv2Raw.get(conv)
  }

  implicit def apply[Raw, Conv](raw2conv: Map[Raw, Conv]): MapConv[Raw, Conv] = MapConv(raw2conv)



  case class PairConv[Conv](convValues: (Conv, Conv))
    extends ValueConv[Boolean, Conv] with ValueRevConv.ExceptionToNoneUnapplyConv[Boolean, Conv]
  {
    def apply(raw: Boolean) = if (!raw) convValues._1 else convValues._2

    def applyRev(conv: Conv) = conv match {
      case convValues._1 => false
      case convValues._2 => true
      case _ => throw new IllegalArgumentException(s"Illegal conv value ${conv}")
    }
  }

  implicit def apply[Conv](convValues: (Conv, Conv)): PairConv[Conv] = PairConv(convValues)



  case class RangeConv(val range: Range) extends ValueConv[Int, Int] {
    def apply(raw: Int) = {
      if (range.contains(raw)) raw
      else throw new IllegalArgumentException(s"Raw value ${raw} out of range")
    }

    def applyRev(conv: Int) = {
      if (range.contains(conv)) conv
      else throw new IllegalArgumentException(s"Conv value ${conv} out of range")
    }

    def unapply(conv: Int) = {
      if (range.contains(conv)) Some(conv)
      else None
    }
  }

  implicit def apply(range: Range): RangeConv = RangeConv(range)
}



case class LinearConv[@specialized(Int, Long, Float, Double) Raw](offset: Double = 0, scale: Double = 1)(implicit rawNT: NumType[Raw])
  extends ValueConv[Raw, Double] with ValueRevConv.SimpleUnapplyConv[Raw, Double]
{
  def apply(raw: Raw) = offset + scale * rawNT.toDouble(raw)

  def applyRev(conv: Double) = rawNT.from((conv - offset) / scale)
}


case class IntMultOffsetConv[@specialized(Int, Long) Raw, @specialized(Int, Long) Conv](offset: Conv = 0, scale: Conv = 1)(implicit rawNT: NumType[Raw], convNT: NumType[Conv])
  extends ValueConv[Raw, Conv] with ValueRevConv.SimpleUnapplyConv[Raw, Conv]
{
  def apply(raw: Raw) = convNT.from(convNT.toLong(offset) + rawNT.toLong(raw) * convNT.toLong(scale))

  def applyRev(conv: Conv) = rawNT.from((convNT.toLong(conv) - convNT.toLong(offset)) / convNT.toLong(scale))
}


case class IntDivOffsetConv[@specialized(Int, Long) Raw, @specialized(Int, Long) Conv](offset: Conv = 0, divider: Conv = 1)(implicit rawNT: NumType[Raw], convNT: NumType[Conv])
  extends ValueConv[Raw, Conv] with ValueRevConv.SimpleUnapplyConv[Raw, Conv]
{
  def apply(raw: Raw) = convNT.from(convNT.toLong(offset) + rawNT.toLong(raw) / convNT.toLong(divider))

  def applyRev(conv: Conv) = rawNT.from((convNT.toLong(conv) - convNT.toLong(offset)) * convNT.toLong(divider))
}
