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


class ByteOps(val x: Byte) extends AnyVal {
	def asUnsigned: Int = x.toInt & 0xff
}


class ShortOps(val x: Short) extends AnyVal {
	def asUnsigned: Int = x.toInt & 0xffff
}


class IntOps(val x: Int) extends AnyVal {
	def asUnsigned: Long = x.toLong & 0xffffffffL

	def toUnsignedByte: Byte = {
		if (x < 0) throw new IllegalArgumentException("Can't store negative value as unsigned")
		if (x > 0xff) throw new IllegalArgumentException("Value too large for unsigned Byte")
		else x.toByte
	}

	def toUnsignedShort: Short = {
		if (x < 0) throw new IllegalArgumentException("Can't store negative value as unsigned")
		if (x > 0xffff) throw new IllegalArgumentException("Value too large for unsigned Short")
		else x.toShort
	}
}


class LongOps(val x: Long) extends AnyVal {
	def toUnsignedInt: Int = {
		if (x < 0) throw new IllegalArgumentException("Can't store negative value as unsigned")
		if (x > 0xffffffffL) throw new IllegalArgumentException("Value too large for unsigned Int")
		else x.toInt
	}
}
