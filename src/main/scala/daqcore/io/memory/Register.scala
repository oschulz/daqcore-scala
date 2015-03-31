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


trait Register { thisRegister =>
  import Register._

  trait RegBitSelection extends BitSelection {
    def register = thisRegister
  }

  trait RegSingleBit extends Bit with RegBitSelection

  trait RegBitRange extends BitRange with RegBitSelection


  type Fields = Seq[(String, RegBitSelection)]

  abstract class Content {
    def fields: Fields

    protected def getFieldValues(fields: Fields, value: Int): Seq[(String, Int)] =
      fields map { _ match { case (name, bits) => name -> value.getBits(bits) } }

    protected def getFieldValues(fields: Fields, value: Long): Seq[(String, Long)] =
      fields map { _ match { case (name, bits) => name -> value.getBits(bits) } }

    def apply(value: Byte): Seq[(String, Int)] = getFieldValues(fields, value.asUnsigned)
    def apply(value: Short): Seq[(String, Int)] = getFieldValues(fields, value.asUnsigned)
    def apply(value: Int): Seq[(String, Int)] = getFieldValues(fields, value)
    def apply(value: Long): Seq[(String, Long)] = getFieldValues(fields, value)
  }


  class AllContent extends Content {
    def fields: Fields = for {
      method <- getClass.getDeclaredMethods.toList
      if method.getParameterTypes.isEmpty
      if classOf[RegBitSelection].isAssignableFrom(method.getReturnType)
    } yield {
      method.getName -> method.invoke(this).asInstanceOf[RegBitSelection]
    }
  }
  def all = new AllContent


  def nBits: Int

  require ((nBits > 0) && (nBits <= 64) && (nBits % 8 == 0), "Illegal number of bits for register" )
}


object Register {
}


class SimpleRegister(val nBits: Int) extends Register {
  thisRegister =>

  case class RegBit(n: Int = 0) extends RegSingleBit

  case class RegBits(bits: Range) extends RegBitRange
}
