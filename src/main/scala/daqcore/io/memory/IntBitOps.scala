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


class ByteBitOps(val x: Byte) extends AnyVal {
	def getBits(bitSel: BitSelection): Byte = bitSel.getBits(x)
	def setBits(bitSel: BitSelection, value: Byte): Byte = bitSel.setBits(x, value)
	def setBits(bitSel: BitSelection, value: Int): Byte = bitSel.setBits(x, value)

	def getBits(bits: Range): Byte = BitRange(bits).getBits(x)
	def setBits(bits: Range, value: Byte): Byte = BitRange(bits).setBits(x, value)
	def setBits(bits: Range, value: Int): Byte = BitRange(bits).setBits(x, value)

	def getBit(bit: Bit): Boolean = bit.getBit(x)
	def setBit(bit: Bit, value: Boolean): Byte = bit.setBit(x, value)

	def getBit(n: Int): Boolean = getBit(Bit(n))
	def setBit(n: Int, value: Boolean): Byte = setBit(Bit(n), value)
}


class ShortBitOps(val x: Short) extends AnyVal {
	def getBits(bitSel: BitSelection): Short = bitSel.getBits(x)
	def setBits(bitSel: BitSelection, value: Short): Short = bitSel.setBits(x, value)
	def setBits(bitSel: BitSelection, value: Int): Short = bitSel.setBits(x, value)

	def getBits(bits: Range): Short = BitRange(bits).getBits(x)
	def setBits(bits: Range, value: Short): Short = BitRange(bits).setBits(x, value)
	def setBits(bits: Range, value: Int): Short = BitRange(bits).setBits(x, value)

	def getBit(bit: Bit): Boolean = bit.getBit(x)
	def setBit(bit: Bit, value: Boolean): Short = bit.setBit(x, value)

	def getBit(n: Int): Boolean = getBit(Bit(n))
	def setBit(n: Int, value: Boolean): Short = setBit(Bit(n), value)
}


class IntBitOps(val x: Int) extends AnyVal {
	def getBits(bitSel: BitSelection): Int = bitSel.getBits(x)
	def setBits(bitSel: BitSelection, value: Int): Int = bitSel.setBits(x, value)

	def getBits(bits: Range): Int = BitRange(bits).getBits(x)
	def setBits(bits: Range, value: Int): Int = BitRange(bits).setBits(x, value)

	def getBit(bit: Bit): Boolean = bit.getBit(x)
	def setBit(bit: Bit, value: Boolean): Int = bit.setBit(x, value)

	def getBit(n: Int): Boolean = getBit(Bit(n))
	def setBit(n: Int, value: Boolean): Int = setBit(Bit(n), value)
}


class LongBitOps(val x: Long) extends AnyVal {
	def getBits(bitSel: BitSelection): Long = bitSel.getBits(x)
	def setBits(bitSel: BitSelection, value: Long): Long = bitSel.setBits(x, value)
	def setBits(bitSel: BitSelection, value: Int): Long = bitSel.setBits(x, value)

	def getBits(bits: Range): Long = BitRange(bits).getBits(x)
	def setBits(bits: Range, value: Long): Long = BitRange(bits).setBits(x, value)
	def setBits(bits: Range, value: Int): Long = BitRange(bits).setBits(x, value)

	def getBit(bit: Bit): Boolean = bit.getBit(x)
	def setBit(bit: Bit, value: Boolean): Long = bit.setBit(x, value)

	def getBit(n: Int): Boolean = getBit(Bit(n))
	def setBit(n: Int, value: Boolean): Long = setBit(Bit(n), value)
}
