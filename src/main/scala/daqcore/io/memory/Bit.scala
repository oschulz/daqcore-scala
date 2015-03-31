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



trait BitSelection extends Ordered[BitSelection] {
  import BitSelection._

  def valueMask: Int
  def valueMaskLong: Long

  def firstBit: Int

  def compare(that: BitSelection) = this.firstBit compare that.firstBit
  
  def mask = valueMask << firstBit
  def maskLong = valueMaskLong << firstBit


  def getBits(from: Byte): Byte = getBits(from.asUnsigned).toUnsignedByte

  def getBits(from: Short): Short = getBits(from.asUnsigned).toUnsignedShort

  def getBits(from: Int): Int = (from & mask) >>> firstBit

  def getBits(from: Long): Long = (from & maskLong) >>> firstBit


  def setBits(of: Byte, to: Byte): Byte = setBits(of.asUnsigned, to.asUnsigned).toUnsignedByte
  def setBits(of: Byte, to: Int): Byte = setBits(of.asUnsigned, to).toUnsignedByte

  def setBits(of: Short, to: Short): Short = setBits(of.asUnsigned, to.asUnsigned).toUnsignedShort
  def setBits(of: Short, to: Int): Short = setBits(of.asUnsigned, to).toUnsignedShort

  def setBits(of: Int, to: Int): Int = {
    require ((to & ~valueMask) == 0, "Value does not fit into selected bits")
    (of & ~mask) | (to << firstBit)
  }

  def setBits(of: Long, to: Long): Long = {
    require ((to & ~valueMaskLong) == 0, "Value does not fit into selected bits")
    (of & ~maskLong) | (to << firstBit)
  }
  def setBits(of: Long, to: Int): Long = setBits(of, to.asUnsigned)


  def asString: String
}


object BitSelection {
  val nByteBits = (8 * sizeOf[Byte])
  val nShortBits = (8 * sizeOf[Short])
  val nIntBits = (8 * sizeOf[Int])
  val nLongBits = (8 * sizeOf[Long])

  def apply() = SimpleBit()
  def apply(n: Int) = SimpleBit(n)
}



trait Bit extends BitSelection {
  import BitSelection._

  def n:Int

  require (n >= 0, "Bit number must not be negative")

  def valueMask = 1
  def valueMaskLong = 1L
  
  def firstBit = n


  def getBit(from: Byte): Boolean = getBit(from.asUnsigned)

  def getBit(from: Short): Boolean = getBit(from.asUnsigned)

  def getBit(from: Int): Boolean = (from & (1 << n)) != 0

  def getBit(from: Long): Boolean = (from & (1L << n)) != 0


  def setBit(of: Byte, to:Boolean): Byte = setBit(of.asUnsigned, to).toUnsignedByte

  def setBit(of: Short, to:Boolean): Short = setBit(of.asUnsigned, to).toUnsignedShort

  def setBit(of: Int, to:Boolean): Int = if (to) of | mask else of & ~mask

  def setBit(of: Long, to:Boolean): Long = if (to) of | maskLong else of & ~maskLong


  def apply(value: Byte): Boolean = getBit(value)

  def apply(value: Short): Boolean = getBit(value)

  def apply(value: Int): Boolean = getBit(value)

  def apply(value: Long): Boolean = getBit(value)


  def asString = n.toString
}


object Bit {
  def apply() = SimpleBit()
  def apply(n: Int) = SimpleBit(n)
}


case class SimpleBit(n: Int = 0) extends Bit



trait BitRange extends BitSelection {
  import BitSelection._

  def bits: Range

  def from: Int = bits.head
  def to: Int = bits.end

  def size = bits.size
  def length = bits.length

  def valueMask = (1 << size) - 1
  def valueMaskLong = (1L << size) - 1L
  
  def firstBit = from

  def asString = if (size > 1) "%s..%s".format(from, to) else from.toString

  override def toString = s"${getClass.getSimpleName}(${asString})"


  def apply(value: Byte): Byte = getBits(value)

  def apply(value: Short): Short = getBits(value)

  def apply(value: Int): Int = getBits(value)

  def apply(value: Long): Long = getBits(value)
}


object BitRange {
  def apply() = SimpleBitRange(0 to 0)
  def apply(bits: Range) = SimpleBitRange(bits)
}


case class SimpleBitRange(bits: Range) extends BitRange
