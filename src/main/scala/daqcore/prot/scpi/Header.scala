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


package daqcore.prot.scpi

import daqcore.util._


sealed abstract class Header extends ByteCharSeqFragment {
  def ! = Command(this)
  def !(params: ByteCharSeq*) = Command(this, params: _*)
  def ? = Query(this)
  def ?(params: ByteCharSeq*) = Query(this, params: _*)
}


case class CCQHeader(mnemonic: String) extends Header {
  require(mnemonic == mnemonic.toUpperCase)
  def charSeq = ByteCharSeq("*") ++ ByteCharSeq(mnemonic)
  override def toString = "*" + mnemonic
}


sealed abstract class ICHeader extends Header {
  def ~(part: ICHeaderPart): ICHeader
}


case class ICHeaderAbs(parts: ICHeaderPart*) extends ICHeader {
  def charSeq = ByteCharSeq(":") ++ parts.map(_.charSeq).reduceLeft(_ ++ ByteCharSeq(":") ++ _)
  def ~(part: ICHeaderPart): ICHeaderAbs = ICHeaderAbs((parts ++ Seq(part)) :_*)
  override def toString = ":" + parts.map(_.toString).mkString(":")
}

case class ICHeaderRel(parts: ICHeaderPart*) extends ICHeader {
  def charSeq = parts.map(_.charSeq).reduceLeft(_ ++ ByteCharSeq(":") ++ _)
  def unary_~ = ICHeaderAbs(parts :_*)
  def ~(part: ICHeaderPart): ICHeaderRel = ICHeaderRel((parts ++ Seq(part)) :_*)
  override def toString = parts.map(_.toString) mkString(":")
}


case class ICHeaderPart(mnem: Mnemonic, suffix: Int = 1) {
  require(suffix >= 1)
  def charSeq = mnem.charSeq ++ (if (suffix > 1) ByteCharSeq(suffix.toString) else ByteCharSeq(""))
  def unary_~ = ICHeaderAbs(this)
  def ~(that: ICHeaderPart) = ICHeaderRel(this, that)
  override def toString = mnem.toString ++ (if (suffix > 1) suffix.toString else "")
}
