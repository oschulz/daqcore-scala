// Copyright (C) 2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.math

object ZigZagEnc {
  def encode(x: Byte): Byte = encode(x.toInt).toByte
  def decode(x: Byte): Byte = decode(x.toInt).toByte

  def encode(x: Short): Short = encode(x.toInt).toShort
  def decode(x: Short): Short = decode(x.toInt).toShort

  def encode(x: Int): Int = (x << 1) ^ (x >> 31)
  def decode(x: Int): Int = (x >>> 1) ^ (-(x & 1))

  def encode(x: Long): Long = (x << 1) ^ (x >> 63)
  def decode(x: Long): Long = (x >>> 1) ^ (-(x & 1))
}
