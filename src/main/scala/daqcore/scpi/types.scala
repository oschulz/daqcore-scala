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


object NR1 {
  def unapply(bs: ByteCSeq) : Option[Int] =
    try { Some(bs.toString.toInt) } catch { case e: NumberFormatException => None }
}


object NRf {
  def unapply(bs: ByteCSeq) : Option[Double] =
    try { Some(bs.toString.toDouble) } catch { case e: NumberFormatException => None }
}


object SRD {
  protected val sqString = """'([^']*)'""".r
  protected val dqString = """"([^']*)"""".r

  def unapply(bs: ByteCSeq) : Option[String] = bs match {
    case sqString(contents) => Some(contents)
    case dqString(contents) => Some(contents)
    case _ => None
  }
}


object NR1Seq {
  def unapply(seq: Seq[ByteCSeq]) : Option[Seq[Int]] = try {
    Some(seq map {arg => val NR1(value) = arg; value})
  } catch {
    case e: ClassCastException => None
    case e: NumberFormatException => None
  }
}


object BlockData {
  def apply(data: IndexedSeq[Byte]) : ByteCSeq = {
    val tag = "#".getBytes
    val sizeStr = data.size.toString.getBytes
    val sizeSizeStr = sizeStr.size.toString.getBytes
    
    ByteCSeq(tag ++ sizeSizeStr ++ sizeStr ++ data)
  }


  def unapply(bs: ByteCSeq) : Option[IndexedSeq[Byte]] = try {
    val bytes = SCPIParser.parseAll(SCPIParser.blockData, bs).get
    Some(bytes)
  } catch {
    case _ => None
  }
}
