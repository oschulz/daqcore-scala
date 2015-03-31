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
  private[memory] def getBits[A](bitSel: BitSelection[A], from: Int): Int =
    (from & bitSel.mask) >>> bitSel.firstBit

  private[memory] def getBits[A](bitSel: BitSelection[A], from: Long): Long =
    (from & bitSel.maskLong) >>> bitSel.firstBit

  private[memory] def setBits[A](bitSel: BitSelection[A], of: Int, to: Int): Int = {
    require ((to & ~bitSel.valueMask) == 0, "Value does not fit into selected bits")
    (of & ~bitSel.mask) | (to << bitSel.firstBit)
  }

  private[memory] def setBits[A](bitSel: BitSelection[A], of: Long, to: Long): Long = {
    require ((to & ~bitSel.valueMaskLong) == 0, "Value does not fit into selected bits")
    (of & ~bitSel.maskLong) | (to << bitSel.firstBit)
  }
}


class ByteBitSelectionOps(val bitSel: BitSelection[Byte]) extends AnyVal {
  def getBits(from: Byte): Byte = BitSelectionOps.getBits(bitSel, from.asUnsigned).toUnsignedByte
  def setBits(of: Byte, to: Byte): Byte = BitSelectionOps.setBits(bitSel, of.asUnsigned, to.asUnsigned).toUnsignedByte
  def setBits(of: Byte, to: Int): Byte = BitSelectionOps.setBits(bitSel, of.asUnsigned, to).toUnsignedByte
}


class ShortBitSelectionOps(val bitSel: BitSelection[Short]) extends AnyVal {
  def getBits(from: Short): Short = BitSelectionOps.getBits(bitSel, from.asUnsigned).toUnsignedShort
  def setBits(of: Short, to: Short): Short = BitSelectionOps.setBits(bitSel, of.asUnsigned, to.asUnsigned).toUnsignedShort
  def setBits(of: Short, to: Int): Short = BitSelectionOps.setBits(bitSel, of.asUnsigned, to).toUnsignedShort
}


class IntBitSelectionOps(val bitSel: BitSelection[Int]) extends AnyVal {
  def getBits(from: Int): Int = BitSelectionOps.getBits(bitSel, from)
  def setBits(of: Int, to: Int): Int = BitSelectionOps.setBits(bitSel, of, to)
}


class LongBitSelectionOps(val bitSel: BitSelection[Long]) extends AnyVal {
  def getBits(from: Long): Long = BitSelectionOps.getBits(bitSel, from)
  def setBits(of: Long, to: Long): Long = BitSelectionOps.setBits(bitSel, of, to)
  def setBits(of: Long, to: Int): Long = BitSelectionOps.setBits(bitSel, of, to.asUnsigned)
}



object BitOps {
  def getBit[A](bit: Bit[A], from: Int): Boolean = (from & (1 << bit.n)) != 0
  def setBit[A](bit: Bit[A], of: Int, to:Boolean): Int = if (to) of | bit.mask else of & ~bit.mask

  def getBit[A](bit: Bit[A], from: Long): Boolean = (from & (1L << bit.n)) != 0
  def setBit[A](bit: Bit[A], of: Long, to:Boolean): Long = if (to) of | bit.maskLong else of & ~bit.maskLong
}


class ByteBitOps(val bit: Bit[Byte]) extends AnyVal {
  def getBit(from: Byte): Boolean = BitOps.getBit(bit, from.asUnsigned)
  def setBit(of: Byte, to:Boolean): Byte = BitOps.setBit(bit, of.asUnsigned, to).toUnsignedByte
  def apply(value: Byte): Boolean = BitOps.getBit(bit, value)
}


class ShortBitOps(val bit: Bit[Short]) extends AnyVal {
  def getBit(from: Short): Boolean = BitOps.getBit(bit, from.asUnsigned)
  def setBit(of: Short, to:Boolean): Short = BitOps.setBit(bit, of.asUnsigned, to).toUnsignedShort
  def apply(value: Short): Boolean = BitOps.getBit(bit, value)
}


class IntBitOps(val bit: Bit[Int]) extends AnyVal {
  def getBit(from: Int): Boolean = BitOps.getBit(bit, from)
  def setBit(of: Int, to:Boolean): Int = BitOps.setBit(bit, of, to)
  def apply(value: Int): Boolean = BitOps.getBit(bit, value)
}


class LongBitOps(val bit: Bit[Long]) extends AnyVal {
  def getBit(from: Long): Boolean = BitOps.getBit(bit, from)
  def setBit(of: Long, to:Boolean): Long = BitOps.setBit(bit, of, to)
  def apply(value: Long): Boolean = BitOps.getBit(bit, value)
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
