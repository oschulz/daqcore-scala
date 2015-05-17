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


package daqcore.io.memory

import scala.language.implicitConversions

import daqcore.util._



final case class BitMaskedInteger[@specialized(Byte, Short, Int, Long) T](value: T, mask: T) {
  def +|(that: BitMaskedInteger[T])(implicit numType: IntegerNumType[T]) = BitMaskedInteger(
    value = numType.bitwiseMerge(this.value, that.value, that.mask),
    mask = numType.bitwiseOr(this.mask, that.mask)
  )

  def maskedValue(implicit numType: IntegerNumType[T]) = numType.bitwiseAnd(value, mask)

  def isMasked(implicit numType: IntegerNumType[T]): Boolean = (mask != numType.unsignedMax)

  def getBits(bitSel: BitSelection[T])(implicit numType: IntegerNumType[T]): T = bitSel.getBits(maskedValue)
  def setBits(bitSel: BitSelection[T], to: T)(implicit numType: IntegerNumType[T]): BitMaskedInteger[T] =
    BitMaskedInteger(bitSel.setBits(value, to), numType.bitwiseOr(mask, bitSel.bitMask))

  def getBits(bits: Range)(implicit numType: IntegerNumType[T]): T = getBits(BitRange[T](bits))
  def setBits(bits: Range, to: T)(implicit numType: IntegerNumType[T]): BitMaskedInteger[T] = setBits(BitRange[T](bits), to)

  def getBit(bit: Bit[T])(implicit numType: IntegerNumType[T]): Boolean = bit.getBit(maskedValue)
  def setBit(bit: Bit[T], to: Boolean)(implicit numType: IntegerNumType[T]): BitMaskedInteger[T] =
    BitMaskedInteger(bit.setBit(value, to), numType.bitwiseOr(mask, bit.bitMask))
  def setBit(bit: Bit[T])(implicit numType: IntegerNumType[T]): BitMaskedInteger[T] =
    BitMaskedInteger(bit.setBit(value), numType.bitwiseOr(mask, bit.bitMask))
  def clearBit(bit: Bit[T])(implicit numType: IntegerNumType[T]): BitMaskedInteger[T] =
    BitMaskedInteger(bit.clearBit(value), numType.bitwiseOr(mask, bit.bitMask))

  def getBit(bit: Int)(implicit numType: IntegerNumType[T]): Boolean = getBit(Bit[T](bit))
  def setBit(bit: Int, to: Boolean)(implicit numType: IntegerNumType[T]): BitMaskedInteger[T] = setBit(Bit[T](bit), to)
  def setBit(bit: Int)(implicit numType: IntegerNumType[T]): BitMaskedInteger[T] = setBit(Bit[T](bit))
  def clearBit(bit: Int)(implicit numType: IntegerNumType[T]): BitMaskedInteger[T] = clearBit(Bit[T](bit))

  override def toString = s"(0x${hex(value)} withBitMask 0x${hex(mask)})"
}


object BitMaskedInteger {
  def apply[@specialized(Byte, Short, Int, Long) T]()(implicit numType: IntegerNumType[T]): BitMaskedInteger[T] =
    BitMaskedInteger(numType.zero, numType.unsignedMax)

  def apply[@specialized(Byte, Short, Int, Long) T](value: T)(implicit numType: IntegerNumType[T]): BitMaskedInteger[T] =
    BitMaskedInteger(value, numType.unsignedMax)

  def apply[@specialized(Byte, Short, Int, Long) T](value: T, bitSel: BitSelection[T])(implicit numType: IntegerNumType[T]): BitMaskedInteger[T] =
    BitMaskedInteger(value, bitSel.bitMask)

  implicit def fromInteger[@specialized(Byte, Short, Int, Long) T](value: T)(implicit numType: IntegerNumType[T]) =
    new BitMaskedInteger(value, numType.unsignedMax)

  class OpsWith[@specialized(Byte, Short, Int, Long) T](x: T) {
    def +|(y: BitMaskedInteger[T])(implicit numType: IntegerNumType[T]) = numType.bitwiseMerge(x.value, y.value, y.mask)
  }
}


case class MemValues[@specialized(Int, Long) Address, @specialized(Byte, Short, Int, Long) Value]
  (values: Map[Address, Value] = Map[Address, Value]())
  extends PartialFunction[Address, Value]
{
  def apply(address: Address): Value = values(address)
  def get(address: Address): Option[Value] = values.get(address)
  def contains(address: Address): Boolean = values.contains(address)
  def isDefinedAt(address: Address): Boolean = values.isDefinedAt(address)
  def isEmpty: Boolean = values.isEmpty

  def ++(xs: Traversable[(Address, Value)]): MemValues[Address, Value] = MemValues(values ++ xs)
  def ++(that: MemValues[Address, Value]): MemValues[Address, Value] = this ++ that.values
  def +(x: (Address, Value)): MemValues[Address, Value] = MemValues(values + x)

  // Maybe later: size(), monadic methods, full Traversable compatibility ...
}

case object MemValues {
  def apply[@specialized(Int, Long) Address, @specialized(Byte, Short, Int, Long) Value](xs: (Address, Value)*): MemValues[Address, Value] =
    MemValues(xs.toMap)
}


case class MaskedMemValues[@specialized(Int, Long) Address, @specialized(Byte, Short, Int, Long) Value]
  (values: Map[Address, BitMaskedInteger[Value]] = Map[Address, BitMaskedInteger[Value]]())
  extends PartialFunction[Address, BitMaskedInteger[Value]]
{
  protected type MMVMap = Map[Address, BitMaskedInteger[Value]]

  def apply(address: Address): BitMaskedInteger[Value] = values(address)
  def get(address: Address): Option[BitMaskedInteger[Value]] = values.get(address)
  def contains(address: Address): Boolean = values.contains(address)
  def isDefinedAt(address: Address): Boolean = values.isDefinedAt(address)
  def isEmpty: Boolean = values.isEmpty

  def ++(xs: Traversable[(Address, BitMaskedInteger[Value])])(implicit numType: IntegerNumType[Value]): MaskedMemValues[Address, Value] = {
    def builder = Map.newBuilder[Address, BitMaskedInteger[Value]]
    builder ++= values
    xs foreach { case (thatAddr, thatValue) =>
      builder += (( thatAddr, values.get(thatAddr) map (_ +| thatValue) getOrElse thatValue ))
    }
    MaskedMemValues(builder.result)
  }

  def ++(that: MaskedMemValues[Address, Value])(implicit numType: IntegerNumType[Value]): MaskedMemValues[Address, Value] =
    this ++ that.values

  def +(x: (Address, BitMaskedInteger[Value]))(implicit numType: IntegerNumType[Value]): MaskedMemValues[Address, Value] = {
    val (address, value) = x
    val combinedValue = values.get(address) map (_ +| value) getOrElse value
    MaskedMemValues(values + (address -> combinedValue))
  }

  // Maybe later: size(), monadic methods, full Traversable compatibility ...
}

case object MaskedMemValues {
  def apply[@specialized(Int, Long) Address, @specialized(Byte, Short, Int, Long) Value](xs: (Address, BitMaskedInteger[Value])*): MaskedMemValues[Address, Value] =
    MaskedMemValues(xs.toMap)
}
