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


package daqcore.scpi

import daqcore.util._


sealed abstract class Mnemonic extends SCPIFragment {
}


object Mnemonic {
  val mnemExp = """^([A-Z]+)([a-z]+)?$""".r
  
  def apply(mnem: String): SpecMnemonic = mnem match {
    case mnemExp(short, rest) => SpecMnemonic(short, if (rest!=null) short+rest else short)
    case _ => throw new IllegalArgumentException("Not a valid mnemonic: \"%s\"".format(mnem))
  }
}
  
case class RecMnemonic(charSeq: ByteCSeq) extends Mnemonic {
  override def hashCode = charSeq.toString.hashCode
    override def canEqual(that: Any) = that.isInstanceOf[Mnemonic]
    override def equals(that: Any) = canEqual(that) && (that match {
    case that:RecMnemonic => that.charSeq == this.charSeq
    case that:SpecMnemonic => that == this
    case _ => false
  })
  
  override def toString = charSeq.toString
}


case class SpecMnemonic(short:String, long:String) extends Mnemonic {
  def charSeq = ByteCSeq(short)

  override def hashCode = short.hashCode
    def canEqual(that: Any) = that.isInstanceOf[Mnemonic]
    override def equals(that: Any) = canEqual(that) && (that match {
    case that:RecMnemonic => {
      val thatStr = that.charSeq.toString
      ( (thatStr == short.toLowerCase) || (thatStr == long.toLowerCase) ||
      (thatStr == short.toUpperCase) || (thatStr == long.toUpperCase) )
    }
    case that:SpecMnemonic => ((that.short == this.short) && (that.long == this.long))
  })
  
  def apply(suffix: Int) = ICHeaderPart(this, suffix)
  def unapply(part: ICHeaderPart): Option[Int] = {
    if (part.mnem == this) Some(part.suffix)
    else None
  }
  
  override def toString = long
}
