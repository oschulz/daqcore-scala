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


class MemRegion(val from: MemRegion.MemAddress, val until: MemRegion.MemAddress, val parent: Option[MemRegion] = None) {
  thisRegion =>

  import MemRegion._
  import Register._

  def length = until - from
  def size = length

  trait MemRegister[T] extends Register[T] {
    thisMemRegister =>

    def region = thisRegion

    def addr: MemAddress

    trait MemBitSelection extends RegBitSelection {
      override def register: MemRegister[T] = thisMemRegister
    }

    trait MemSingleBit extends RegSingleBit with MemBitSelection

    trait MemBitRange extends RegBitRange with MemBitSelection
  }



  trait ReadableRegister[T] extends MemRegister[T] { thisRegister =>
    trait ReadableBitSelection extends MemBitSelection
    trait ReadableBit extends MemSingleBit with ReadableBitSelection

    case class ROBit(n: Int) extends MemSingleBit with ReadableBitSelection
    case class ROBitRange(bits: Range) extends MemBitRange with ReadableBitSelection

    class ReadableContent extends Content {
      type Fields = Seq[(String, ReadableBitSelection)]

      def fields: Fields = for {
        method <- thisRegister.getClass.getDeclaredMethods.toList
        if method.getParameterTypes.isEmpty
        if classOf[ReadableBitSelection].isAssignableFrom(method.getReturnType)
      } yield {
        method.getName -> method.invoke(thisRegister).asInstanceOf[ReadableBitSelection]
      }
    }
    def r = new ReadableContent
  }


  trait WriteableRegister[T] extends MemRegister[T] { thisRegister =>
    trait WriteableBitSelection extends MemBitSelection
    trait WriteableBit extends MemSingleBit with WriteableBitSelection

    case class WOBit(n: Int) extends MemSingleBit with WriteableBitSelection
    case class WOBitRange(bits: Range) extends MemBitRange with WriteableBitSelection

    case class COBit(n: Int) extends MemSingleBit with WriteableBitSelection

    case class SOBit(n: Int) extends MemSingleBit with WriteableBitSelection

    class WriteableContent extends Content {
      type Fields = Seq[(String, WriteableBitSelection)]

      def fields = for {
        method <- thisRegister.getClass.getDeclaredMethods.toList
        if method.getParameterTypes.isEmpty
        if classOf[WriteableBitSelection].isAssignableFrom(method.getReturnType)
      } yield {
        method.getName -> method.invoke(thisRegister).asInstanceOf[WriteableBitSelection]
      }
    }
    def w = new WriteableContent
  }
  
  abstract class RORegister[T] extends ReadableRegister[T]

  abstract class WORegister[T] extends WriteableRegister[T]

 
  abstract class RWRegister[T] extends ReadableRegister[T] with WriteableRegister[T] {
    case class RWBit(n: Int) extends MemSingleBit with ReadableBitSelection with WriteableBitSelection
    case class RWBitRange(bits: Range) extends MemBitRange with ReadableBitSelection with WriteableBitSelection
  }


  abstract class JKRegister[T](val addr: MemAddress) extends ReadableRegister[T] with WriteableRegister[T] {
    protected def jkSet(implicit numType: IntegerNumType[T]) = BitRange[T](0 to (numType.nBits / 2 - 1))
    protected def jkClear(implicit numType: IntegerNumType[T]) = BitRange[T]((numType.nBits / 2) to 31)

    case class RWBit(n: Int)(implicit numType: IntegerNumType[T]) extends MemSingleBit with ReadableBitSelection with WriteableBitSelection {
      require(n < jkSet.bits.end, "Only lower half of bits can be declared in a J/K register")
    }

    case class RWBitRange(bits: Range)(implicit numType: IntegerNumType[T])  extends MemBitRange with ReadableBitSelection with WriteableBitSelection {
      require(bits.end < jkSet.bits.end, "Only lower half of bits can be declared in a J/K register")
    }
  }
  
  class KeyRegister(val addr: MemAddress)
}


object MemRegion {
  type MemAddress = Long
}
