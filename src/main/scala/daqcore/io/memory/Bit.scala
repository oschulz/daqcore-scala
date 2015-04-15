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



trait BitSelection[T] extends Ordered[BitSelection[T]] {
  import BitSelection._

  def firstBit: Int

  def isContigous: Boolean

  def size: Int
  def length: Int

  def compare(that: BitSelection[T]) = this.firstBit compare that.firstBit
  
  def asString: String

  final def getBits[T](from: T)(implicit numType: IntegerNumType[T]): T = {
    require (isContigous, "BitSelection must be contiguous for getBits")
    numType.getBitRange(firstBit, size, from)
  }

  final def setBits[T](of: T, to: T)(implicit numType: IntegerNumType[T]): T = {
    require (isContigous, "BitSelection must be contiguous for setBits")
    numType.setBitRange(firstBit, size, of, to)
  }
}


object BitSelection {
  val nByteBits = (8 * sizeOf[Byte])
  val nShortBits = (8 * sizeOf[Short])
  val nIntBits = (8 * sizeOf[Int])
  val nLongBits = (8 * sizeOf[Long])

  def apply[T](n: Int) = SimpleBit.of[T](n)
  def apply[T](bits: Range) = SimpleBitRange.of[T](bits)
}



trait Bit[T] extends BitSelection[T] {
  import BitSelection._

  def n:Int

  require (n >= 0, "Bit number must not be negative")

  def isContigous = true

  def size = 1
  def length = 1

  def firstBit = n

  def asString = n.toString

  final def getBit(from: T)(implicit numType: IntegerNumType[T]): Boolean = numType.getBit(n, from)

  final def setBit(of: T, to: Boolean)(implicit numType: IntegerNumType[T]): T = numType.setBit(n, of, to)

  final def setBit(of: T)(implicit numType: IntegerNumType[T]): T = numType.setBit(n, of)

  final def clearBit(of: T)(implicit numType: IntegerNumType[T]): T = numType.clearBit(n, of)

  final def apply(value: T)(implicit numType: IntegerNumType[T]): Boolean = getBit(value)
}


object Bit {
  def of[T](n: Int) = SimpleBit.of[T](n)
}



case class SimpleBit[T](n: Int = 0) extends Bit[T]


object SimpleBit {
  def of[T](n: Int) = new SimpleBit[T](n)
}



trait BitRange[T] extends BitSelection[T] {
  import BitSelection._

  def bits: Range

  def from: Int = bits.head
  def to: Int = bits.end

  def isContigous = (bits.step == 1)

  def size = bits.size
  def length = bits.length

  def firstBit = from

  def asString = if (size > 1) "%s..%s".format(from, to) else from.toString

  override def toString = s"${getClass.getSimpleName}(${asString})"

  final def apply(value: T)(implicit numType: IntegerNumType[T]): T = getBits(value)
}


object BitRange {
  def of[T](bits: Range) = SimpleBitRange.of[T](bits)
}



case class SimpleBitRange[T](bits: Range) extends BitRange[T]


object SimpleBitRange {
  def of[T](bits: Range) = new SimpleBitRange[T](bits)
}
