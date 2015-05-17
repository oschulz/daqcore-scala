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

import daqcore.util._


object BitAccessOps {
  private[memory] def getBits[T](bits: Range, from: T)(implicit numType: IntegerNumType[T]): T = numType.getBits(bits, from)
  private[memory] def setBits[T](bits: Range, of: T, to: T)(implicit numType: IntegerNumType[T]): T = numType.setBits(bits, of, to)

  private[memory] def getBit[T](n: Int, from: T)(implicit numType: IntegerNumType[T]): Boolean = numType.getBit(n, from)
  private[memory] def setBit[T](n: Int, of: T, to: Boolean)(implicit numType: IntegerNumType[T]): T = numType.setBit(n, of, to)
  private[memory] def setBit[T](n: Int, of: T)(implicit numType: IntegerNumType[T]): T = numType.setBit(n, of)
  private[memory] def clearBit[T](n: Int, of: T)(implicit numType: IntegerNumType[T]): T = numType.clearBit(n, of)
}



class ByteBitAccessOps(val x: Byte) extends AnyVal {
  def getBits(bitSel: BitSelection[Byte]): Byte = bitSel.getBits(x)
  def setBits(bitSel: BitSelection[Byte], value: Byte): Byte = bitSel.setBits(x, value)

  def getBits(bits: Range): Byte = BitAccessOps.getBits(bits, x)
  def setBits(bits: Range, value: Byte): Byte = BitAccessOps.setBits(bits, x, value)

  def getBit(bit: Bit[Byte]): Boolean = bit.getBit(x)
  def setBit(bit: Bit[Byte], value: Boolean): Byte = bit.setBit(x, value)
  def setBit(bit: Bit[Byte]): Byte = bit.setBit(x)
  def clearBit(bit: Bit[Byte]): Byte = bit.clearBit(x)

  def getBit(n: Int): Boolean = BitAccessOps.getBit(n, x)
  def setBit(n: Int, value: Boolean): Byte = BitAccessOps.setBit(n, x, value)
  def setBit(n: Int): Byte = BitAccessOps.setBit(n, x)
  def clearBit(n: Int): Byte = BitAccessOps.clearBit(n, x)

  def withBitMask(mask: Byte) = BitMaskedInteger[Byte](x, mask)
}


class ShortBitAccessOps(val x: Short) extends AnyVal {
  def getBits(bitSel: BitSelection[Short]): Short = bitSel.getBits(x)
  def setBits(bitSel: BitSelection[Short], value: Short): Short = bitSel.setBits(x, value)

  def getBits(bits: Range): Short = BitAccessOps.getBits(bits, x)
  def setBits(bits: Range, value: Short): Short = BitAccessOps.setBits(bits, x, value)

  def getBit(bit: Bit[Short]): Boolean = bit.getBit(x)
  def setBit(bit: Bit[Short], value: Boolean): Short = bit.setBit(x, value)
  def setBit(bit: Bit[Short]): Short = bit.setBit(x)
  def clearBit(bit: Bit[Short]): Short = bit.clearBit(x)

  def getBit(n: Int): Boolean = BitAccessOps.getBit(n, x)
  def setBit(n: Int, value: Boolean): Short = BitAccessOps.setBit(n, x, value)
  def setBit(n: Int): Short = BitAccessOps.setBit(n, x)
  def clearBit(n: Int): Short = BitAccessOps.clearBit(n, x)

  def withBitMask(mask: Short) = BitMaskedInteger[Short](x, mask)
}


class IntBitAccessOps(val x: Int) extends AnyVal {
  def getBits(bitSel: BitSelection[Int]): Int = bitSel.getBits(x)
  def setBits(bitSel: BitSelection[Int], value: Int): Int = bitSel.setBits(x, value)

  def getBits(bits: Range): Int = BitAccessOps.getBits(bits, x)
  def setBits(bits: Range, value: Int): Int = BitAccessOps.setBits(bits, x, value)

  def getBit(bit: Bit[Int]): Boolean = bit.getBit(x)
  def setBit(bit: Bit[Int], value: Boolean): Int = bit.setBit(x, value)
  def setBit(bit: Bit[Int]): Int = bit.setBit(x)
  def clearBit(bit: Bit[Int]): Int = bit.clearBit(x)

  def getBit(n: Int): Boolean = BitAccessOps.getBit(n, x)
  def setBit(n: Int, value: Boolean): Int = BitAccessOps.setBit(n, x, value)
  def setBit(n: Int): Int = BitAccessOps.setBit(n, x)
  def clearBit(n: Int): Int = BitAccessOps.clearBit(n, x)

  def withBitMask(mask: Int) = BitMaskedInteger[Int](x, mask)
}


class LongBitAccessOps(val x: Long) extends AnyVal {
  def getBits(bitSel: BitSelection[Long]): Long = bitSel.getBits(x)
  def setBits(bitSel: BitSelection[Long], value: Long): Long = bitSel.setBits(x, value)

  def getBits(bits: Range): Long = BitAccessOps.getBits(bits, x)
  def setBits(bits: Range, value: Long): Long = BitAccessOps.setBits(bits, x, value)

  def getBit(bit: Bit[Long]): Boolean = bit.getBit(x)
  def setBit(bit: Bit[Long], value: Boolean): Long = bit.setBit(x, value)
  def setBit(bit: Bit[Long]): Long = bit.setBit(x)
  def clearBit(bit: Bit[Long]): Long = bit.clearBit(x)

  def getBit(n: Int): Boolean = BitAccessOps.getBit(n, x)
  def setBit(n: Int, value: Boolean): Long = BitAccessOps.setBit(n, x, value)
  def setBit(n: Int): Long = BitAccessOps.setBit(n, x)
  def clearBit(n: Int): Long = BitAccessOps.clearBit(n, x)

  def withBitMask(mask: Long) = BitMaskedInteger[Long](x, mask)
}
