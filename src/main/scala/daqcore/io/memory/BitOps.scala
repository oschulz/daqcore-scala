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



object BitSelectionOps {
  private[memory] def getBits[T](bitSel: BitSelection[T], from: T)(implicit numType: IntegerNumType[T]): T = {
    require (bitSel.isContigous, "BitSelection must be contiguous for getBits")
    numType.getBitRange(bitSel.firstBit, bitSel.size, from)
  }

  private[memory] def setBits[T](bitSel: BitSelection[T], of: T, to: T)(implicit numType: IntegerNumType[T]): T = {
    require (bitSel.isContigous, "BitSelection must be contiguous for setBits")
    numType.setBitRange(bitSel.firstBit, bitSel.size, of, to)
  }
}


class ByteBitSelectionOps(val bitSel: BitSelection[Byte]) extends AnyVal {
  def getBits(from: Byte): Byte = BitSelectionOps.getBits(bitSel, from)

  def setBits(of: Byte, to: Byte): Byte = BitSelectionOps.setBits(bitSel, of, to)
}


class ShortBitSelectionOps(val bitSel: BitSelection[Short]) extends AnyVal {
  def getBits(from: Short): Short = BitSelectionOps.getBits(bitSel, from)

  def setBits(of: Short, to: Short): Short = BitSelectionOps.setBits(bitSel, of, to)
}


class IntBitSelectionOps(val bitSel: BitSelection[Int]) extends AnyVal {
  def getBits(from: Int): Int = BitSelectionOps.getBits(bitSel, from)

  def setBits(of: Int, to: Int): Int = BitSelectionOps.setBits(bitSel, of, to)
}


class LongBitSelectionOps(val bitSel: BitSelection[Long]) extends AnyVal {
  def getBits(from: Long): Long = BitSelectionOps.getBits(bitSel, from)

  def setBits(of: Long, to: Long): Long = BitSelectionOps.setBits(bitSel, of, to)
}



object BitOps {
  private[memory] def getBit[T](bit: Bit[T], from: T)(implicit numType: IntegerNumType[T]): Boolean = numType.getBit(bit.n, from)

  private[memory] def setBit[T](bit: Bit[T], of: T, to: Boolean)(implicit numType: IntegerNumType[T]): T = numType.setBit(bit.n, of, to)

  private[memory] def setBit[T](bit: Bit[T], of: T)(implicit numType: IntegerNumType[T]): T = numType.setBit(bit.n, of)

  private[memory] def clearBit[T](bit: Bit[T], of: T)(implicit numType: IntegerNumType[T]): T = numType.clearBit(bit.n, of)
}


class ByteBitOps(val bit: Bit[Byte]) extends AnyVal {
  def getBit(from: Byte): Boolean = BitOps.getBit(bit, from)
  def setBit(of: Byte, to: Boolean): Byte = BitOps.setBit(bit, of, to)
  def setBit(of: Byte): Byte = BitOps.setBit(bit, of)
  def clearBit(of: Byte): Byte = BitOps.clearBit(bit, of)
  def apply(value: Byte): Boolean = getBit(value)
}


class ShortBitOps(val bit: Bit[Short]) extends AnyVal {
  def getBit(from: Short): Boolean = BitOps.getBit(bit, from)
  def setBit(of: Short, to: Boolean): Short = BitOps.setBit(bit, of, to)
  def setBit(of: Short): Short = BitOps.setBit(bit, of)
  def clearBit(of: Short): Short = BitOps.clearBit(bit, of)
  def apply(value: Short): Boolean = getBit(value)
}


class IntBitOps(val bit: Bit[Int]) extends AnyVal {
  def getBit(from: Int): Boolean = BitOps.getBit(bit, from)
  def setBit(of: Int, to: Boolean): Int = BitOps.setBit(bit, of, to)
  def setBit(of: Int): Int = BitOps.setBit(bit, of)
  def clearBit(of: Int): Int = BitOps.clearBit(bit, of)
  def apply(value: Int): Boolean = getBit(value)
}


class LongBitOps(val bit: Bit[Long]) extends AnyVal {
  def getBit(from: Long): Boolean = BitOps.getBit(bit, from)
  def setBit(of: Long, to: Boolean): Long = BitOps.setBit(bit, of, to)
  def setBit(of: Long): Long = BitOps.setBit(bit, of)
  def clearBit(of: Long): Long = BitOps.clearBit(bit, of)
  def apply(value: Long): Boolean = getBit(value)
}



class ByteBitRangeOps(val bitRange: BitRange[Byte]) extends AnyVal {
  def apply(value: Byte): Byte = bitRange.getBits(value)
}


class ShortBitRangeOps(val bitRange: BitRange[Short]) extends AnyVal {
  def apply(value: Short): Short = bitRange.getBits(value)
}


class IntBitRangeOps(val bitRange: BitRange[Int]) extends AnyVal {
  def apply(value: Int): Int = bitRange.getBits(value)
}


class LongBitRangeOps(val bitRange: BitRange[Long]) extends AnyVal {
  def apply(value: Long): Long = bitRange.getBits(value)
}
