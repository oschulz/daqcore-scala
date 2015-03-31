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

  trait MemRegister[A] extends Register[A] {
    thisMemRegister =>

    def region = thisRegion

    def addr: MemAddress

    trait MemBitSelection extends RegBitSelection {
      override def register: MemRegister[A] = thisMemRegister
    }

    trait MemSingleBit extends RegSingleBit with MemBitSelection

    trait MemBitRange extends RegBitRange with MemBitSelection
  }



  trait ReadableRegister[A] extends MemRegister[A] { thisRegister =>
    trait ReadableBitSelection extends MemBitSelection {
        //!!!def get() = thisRegister.get(this)
    }

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

    //!!!def get() = thisRegion.read(addr)
    //!!!def get(bitSel: BitSelection) = thisRegion.read(addr, bitSel)
  }


  trait WriteableRegister[A] extends MemRegister[A] { thisRegister =>
    trait WriteableBitSelection extends MemBitSelection {
      //!!! def set(value: Word) = thisRegister.set(this, value)
    }

    case class WOBit(n: Int) extends MemSingleBit with WriteableBitSelection
    case class WOBitRange(bits: Range) extends MemBitRange with WriteableBitSelection

    case class COBit(n: Int) extends MemSingleBit with WriteableBitSelection {
      //!!! def clear() = thisRegister.set(this, 0)
    }

    case class SOBit(n: Int) extends MemSingleBit with WriteableBitSelection {
      //!!! def set() = thisRegister.set(this, 1)
    }

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

    //!!! def set(value: Word) = thisRegion.write(addr, value)
    //!!! def set(bitSel: BitSelection, value: Word) = thisRegion.write(addr, bitSel, value)

  }
  
  abstract class RORegister[A] extends ReadableRegister[A]

  abstract class WORegister[A] extends WriteableRegister[A]

 
  abstract class RWRegister[A] extends ReadableRegister[A] with WriteableRegister[A] {
    case class RWBit(n: Int) extends MemSingleBit with ReadableBitSelection with WriteableBitSelection
    case class RWBitRange(bits: Range) extends MemBitRange with ReadableBitSelection with WriteableBitSelection
  }


  abstract class JKRegister[A](val addr: MemAddress) extends ReadableRegister[A] with WriteableRegister[A] {
    //protected val jkSet = BitRange(0 to (nBits / 2 - 1))
    //protected val jkClear = BitRange((nBits / 2) to 31)

    case class RWBit(n: Int) extends MemSingleBit with ReadableBitSelection with WriteableBitSelection {
      //require(n < jkSet.bits.end, "Only lower half of bits can be declared in a J/K register")
    }
    case class RWBitRange(bits: Range) extends MemBitRange with ReadableBitSelection with WriteableBitSelection {
      //require(bits.end < jkSet.bits.end, "Only lower half of bits can be declared in a J/K register")
    }
    
    //!!!
    /*override def set(value: Word) = {
      val jkSet(jBits) = value
      val jkClear(kBits) = ~value & jkClear.valueMask
      thisRegion.jkWrite(addr, jBits | kBits)
    }
    
    override def set(bitSel: BitSelection, value: Word) = {
      val bitSel(jkSet(jBits)) = value
      val bitSel(jkClear(kBits)) = ~value & bitSel.valueMask
      val toWrite = jBits | kBits
      thisRegion.jkWrite(addr, toWrite)
    }*/
  }
  
  /*
  class KeyRegister(val addr: MemAddress) {
     def set() = thisRegion.write(addr, 0)
  }*/

}


object MemRegion {
  type MemAddress = Long
}
