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



object RegisterOps {
  private[memory] def getFieldValues[T](fields: Register[T]#Fields, value: T)(implicit numType: IntegerNumType[T]): Seq[(String, T)] =
    fields map { _ match { case (name, bits) => name -> BitSelectionOps.getBits(bits, value) } }
}


class ByteRegisterFieldsOps(val fields: Register[Byte]#Fields) extends AnyVal {
  def apply(value: Byte): Seq[(String, Byte)] = RegisterOps.getFieldValues(fields, value)
}


class ShortRegisterFieldsOps(val fields: Register[Short]#Fields) extends AnyVal {
  def apply(value: Short): Seq[(String, Short)] = RegisterOps.getFieldValues(fields, value)
}


class IntRegisterFieldsOps(val fields: Register[Int]#Fields) extends AnyVal {
  def apply(value: Int): Seq[(String, Int)] = RegisterOps.getFieldValues(fields, value)
}


class LongRegisterFieldsOps(val fields: Register[Long]#Fields) extends AnyVal {
  def apply(value: Long): Seq[(String, Long)] = RegisterOps.getFieldValues(fields, value)
}
