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



trait BitSelection[A] extends Ordered[BitSelection[A]] {
  import BitSelection._

  def valueMask: Int
  def valueMaskLong: Long

  def firstBit: Int

  def compare(that: BitSelection[A]) = this.firstBit compare that.firstBit
  
  def mask = valueMask << firstBit
  def maskLong = valueMaskLong << firstBit

  def asString: String
}


object BitSelection {
  val nByteBits = (8 * sizeOf[Byte])
  val nShortBits = (8 * sizeOf[Short])
  val nIntBits = (8 * sizeOf[Int])
  val nLongBits = (8 * sizeOf[Long])

  def apply[A](n: Int) = SimpleBit.of[A](n)
  def apply[A](bits: Range) = SimpleBitRange.of[A](bits)
}



trait Bit[A] extends BitSelection[A] {
  import BitSelection._

  def n:Int

  require (n >= 0, "Bit number must not be negative")

  def valueMask = 1
  def valueMaskLong = 1L
  
  def firstBit = n

  def asString = n.toString
}


object Bit {
  def of[A](n: Int) = SimpleBit.of[A](n)
}



case class SimpleBit[A](n: Int = 0) extends Bit[A]


object SimpleBit {
  def of[A](n: Int) = new SimpleBit[A](n)
}



trait BitRange[A] extends BitSelection[A] {
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
}


object BitRange {
  def of[A](bits: Range) = SimpleBitRange.of[A](bits)
}



case class SimpleBitRange[A](bits: Range) extends BitRange[A]


object SimpleBitRange {
  def of[A](bits: Range) = new SimpleBitRange[A](bits)
}
