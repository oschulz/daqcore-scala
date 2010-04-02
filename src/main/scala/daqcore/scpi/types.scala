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


/** Integral number. Example: 42 */

object NR1 {
  def apply(value: Int) = ByteCSeq(value.toString)

  def unapply(bs: ByteCSeq) : Option[Int] =
    try { Some(bs.toString.toInt) } catch { case e: NumberFormatException => None }
}


// Fixed-point number with explicit decimal point. Example: 42.3
// object NR2 - not implemented yet


// Floating-point number in scientific notation. Example: 4.23E+02
// object NR3 - not implemented yet


/** Comprises NR1, NR2 and NR3 */
object NRf {
  def apply(value: Double) = ByteCSeq(value.toString)

  def unapply(bs: ByteCSeq) : Option[Double] =
    try { Some(bs.toString.toDouble) } catch { case e: NumberFormatException => None }
}


// Extends NRf with MINimum, MAXimum, DEFault, UP, DOWN, NAN, INFinity, NINF
// object NRfp - NRF+, not implemented yet


// Boolean: 0 | 1 or OFF | ON
// object Bool - not implemented yet


abstract class StringData {
  protected val sqString = """'([^']*)'""".r
  protected val dqString = """"([^"]*)"""".r

  def apply(value: String) = ByteCSeq("\"") ++ ByteCSeq(value) ++ ByteCSeq("\"")

  def unapply(bs: ByteCSeq) : Option[String] = bs match {
    case sqString(contents) => Some(contents)
    case dqString(contents) => Some(contents)
    case _ => None
  }
}

/** String Program Data. String enclosed in single or double quotes */
object SPD extends StringData

/** String Response Data. String enclosed in single or double quotes */
object SRD extends StringData



abstract class CharacterData {
  def apply(mnemonic: SpecMnemonic) = mnemonic.charSeq

  def unapply(bs: ByteCSeq) : Option[RecMnemonic] =
    try { Some(RecMnemonic(bs)) } catch { case _ => None }
}

/** Character Program Data. Unquoted mnemonics in short
  * or long form */
object CPD extends CharacterData


/** Character Response Data. Unquoted mnemonics, only the short form
  * is returned. */
object CRD extends CharacterData


// Arbitrary ASCII Response Data. Undelimited 7-bit ASCII data with implied
// message terminator.
// object AARD - not implemented yet


/** Block Data. Binary encoded opaque block data. I. Only the definite length
  * variant (starting with #[1-9]) is implemented so far. The indefinite length
  * variant (starting with #0) has an implied message terminator and is not
  * implemented yet */

object BlockData {
  def apply(data: IndexedSeq[Byte]) : ByteCSeq = {
    val tag = "#".getBytes
    val sizeStr = data.size.toString.getBytes
    val sizeSizeStr = sizeStr.size.toString.getBytes
    
    ByteCSeq(tag ++ sizeSizeStr ++ sizeStr ++ data)
  }


  def unapply(bs: ByteCSeq) : Option[IndexedSeq[Byte]] = try {
    val parser = SCPIParser()
    Some(parser.parseAll(parser.blockDataBytes, bs).get)
  } catch {
    case _ => None
  }
}


trait SCPIFragment {
  def charSeq: ByteCSeq
}
