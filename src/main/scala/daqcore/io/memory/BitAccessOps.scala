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


object BitAccessOps{
}



class ByteBitAccessOps(val x: Byte) extends AnyVal {
	def getBits(bitSel: BitSelection[Byte]): Byte = bitSel.getBits(x)
	def setBits(bitSel: BitSelection[Byte], value: Byte): Byte = bitSel.setBits(x, value)
	def setBits(bitSel: BitSelection[Byte], value: Int): Byte = bitSel.setBits(x, value)

	def getBits(bits: Range): Byte = BitRange.of[Byte](bits).getBits(x)
	def setBits(bits: Range, value: Byte): Byte = BitRange.of[Byte](bits).setBits(x, value)
	def setBits(bits: Range, value: Int): Byte = BitRange.of[Byte](bits).setBits(x, value)

	def getBit(bit: Bit[Byte]): Boolean = bit.getBit(x)
	def setBit(bit: Bit[Byte], value: Boolean): Byte = bit.setBit(x, value)

	def getBit(n: Int): Boolean = getBit(Bit.of[Byte](n))
	def setBit(n: Int, value: Boolean): Byte = setBit(Bit.of[Byte](n), value)
}


class ShortBitAccessOps(val x: Short) extends AnyVal {
	def getBits(bitSel: BitSelection[Short]): Short = bitSel.getBits(x)
	def setBits(bitSel: BitSelection[Short], value: Short): Short = bitSel.setBits(x, value)
	def setBits(bitSel: BitSelection[Short], value: Int): Short = bitSel.setBits(x, value)

	def getBits(bits: Range): Short = BitRange.of[Short](bits).getBits(x)
	def setBits(bits: Range, value: Short): Short = BitRange.of[Short](bits).setBits(x, value)
	def setBits(bits: Range, value: Int): Short = BitRange.of[Short](bits).setBits(x, value)

	def getBit(bit: Bit[Short]): Boolean = bit.getBit(x)
	def setBit(bit: Bit[Short], value: Boolean): Short = bit.setBit(x, value)

	def getBit(n: Int): Boolean = getBit(Bit.of[Short](n))
	def setBit(n: Int, value: Boolean): Short = setBit(Bit.of[Short](n), value)
}


class IntBitAccessOps(val x: Int) extends AnyVal {
	def getBits(bitSel: BitSelection[Int]): Int = bitSel.getBits(x)
	def setBits(bitSel: BitSelection[Int], value: Int): Int = bitSel.setBits(x, value)

	def getBits(bits: Range): Int = BitRange.of[Int](bits).getBits(x)
	def setBits(bits: Range, value: Int): Int = BitRange.of[Int](bits).setBits(x, value)

	def getBit(bit: Bit[Int]): Boolean = bit.getBit(x)
	def setBit(bit: Bit[Int], value: Boolean): Int = bit.setBit(x, value)

	def getBit(n: Int): Boolean = getBit(Bit.of[Int](n))
	def setBit(n: Int, value: Boolean): Int = setBit(Bit.of[Int](n), value)
}


class LongBitAccessOps(val x: Long) extends AnyVal {
	def getBits(bitSel: BitSelection[Long]): Long = bitSel.getBits(x)
	def setBits(bitSel: BitSelection[Long], value: Long): Long = bitSel.setBits(x, value)
	def setBits(bitSel: BitSelection[Long], value: Int): Long = bitSel.setBits(x, value)

	def getBits(bits: Range): Long = BitRange.of[Long](bits).getBits(x)
	def setBits(bits: Range, value: Long): Long = BitRange.of[Long](bits).setBits(x, value)
	def setBits(bits: Range, value: Int): Long = BitRange.of[Long](bits).setBits(x, value)

	def getBit(bit: Bit[Long]): Boolean = bit.getBit(x)
	def setBit(bit: Bit[Long], value: Boolean): Long = bit.setBit(x, value)

	def getBit(n: Int): Boolean = getBit(Bit.of[Long](n))
	def setBit(n: Int, value: Boolean): Long = setBit(Bit.of[Long](n), value)
}
