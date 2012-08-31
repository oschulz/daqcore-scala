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


trait UnsignedType[Type, Repr] {
  def apply(x: Type): Repr
  def unapply(x: Repr): Option[Type]
}


object UnsignedByte extends UnsignedType[Byte, Short] {
  def filter: Short = 0xff
  def apply(x: Byte): Short = (x.toInt & filter).toShort
  def unapply(x: Short): Option[Byte] = {
    val y = x.toByte
    if (apply(y) == x) Some(y) else None
  }
}


object UnsignedShort extends UnsignedType[Short, Int] {
  def filter: Int = 0xffff
  def apply(x: Short): Int = x.toInt & filter
  def unapply(x: Int): Option[Short] = {
    val y = x.toShort
    if (apply(y) == x) Some(y) else None
  }
}


object UnsignedInt extends UnsignedType[Int, Long] {
  def filter: Long = 0xffffffffl
  def apply(x: Int): Long = x.toLong & filter
  def unapply(x: Long): Option[Int] = {
    val y = x.toInt
    if (apply(y) == x) Some(y) else None
  }
}
