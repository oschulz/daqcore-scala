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


package daqcore.io.prot.scpi

import daqcore.util._


sealed abstract class Mnemonic extends HasByteRep {
  def getByteCharSeq: ByteCharSeq
  override def getBytes = getByteCharSeq.getBytes
  def putBytes(builder: ByteStringBuilder) = getByteCharSeq.putBytes(builder)
}


object Mnemonic {
  val mnemExp = """^([A-Z]+)([a-z]+)?$""".r
  
  def apply(mnem: String): SpecMnemonic = mnem match {
    case mnemExp(short, rest) => SpecMnemonic(short, if (rest!=null) short+rest else short)
    case _ => throw new IllegalArgumentException("Not a valid mnemonic: \"%s\"".format(mnem))
  }
}
  
case class RecMnemonic(bytes: ByteCharSeq) extends Mnemonic {
  override def hashCode = getByteCharSeq.toString.hashCode
    override def canEqual(that: Any) = that.isInstanceOf[Mnemonic]
    override def equals(that: Any) = canEqual(that) && (that match {
    case that:RecMnemonic => that.getByteCharSeq == this.getByteCharSeq
    case that:SpecMnemonic => that == this
    case _ => false
  })
  
  def getByteCharSeq = bytes

  override def toString = getByteCharSeq.toString
}


case class SpecMnemonic(short:String, long:String) extends Mnemonic {
  override def hashCode = short.hashCode
    def canEqual(that: Any) = that.isInstanceOf[Mnemonic]
    override def equals(that: Any) = canEqual(that) && (that match {
    case that:RecMnemonic => {
      val thatStr = that.getByteCharSeq.toString
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
  
  def getByteCharSeq = ByteCharSeq(short)
  
  override def toString = long
}
