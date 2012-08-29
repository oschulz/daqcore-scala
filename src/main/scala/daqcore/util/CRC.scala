// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


object CRC_CCITT {
  def update(byte: Byte, initial: Short = 0): Short = {
    val polynomial: Int = 0x1021
    var crc: Int = initial
    var bitSelector: Int = 0x80
    while (bitSelector > 0) {
      val crcMsb: Boolean = (crc & 0x8000) > 0
      val selBit: Boolean = (byte & bitSelector) > 0
      crc = crc << 1
      if (crcMsb ^ selBit) crc = crc ^ polynomial
      bitSelector = bitSelector >> 1
      crc &= 0xffff
    }
    
    crc.toShort
  }
}
