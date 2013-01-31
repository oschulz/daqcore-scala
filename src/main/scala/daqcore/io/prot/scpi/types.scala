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


trait SCPIType[A] {
  def apply(value: A): ByteCharSeq

  def unapply(bs: ByteCharSeq) : Option[A]
}


/** Integral number. Example: 42 */

object NR1 extends SCPIType[Int] {
  def apply(value: Int) = ByteCharSeq(value.toString)

  def unapply(bs: ByteCharSeq) : Option[Int] =
    try { Some(bs.toString.toInt) } catch { case e: NumberFormatException => None }
}


object NR1Seq {
  def apply(values: Int*) = for { x <- values } yield NR1(x)

  def unapplySeq(args: Seq[ByteCharSeq]): Option[Seq[Int]] = try {
    Some( for { arg <- args } yield
      { val NR1(i) = arg; i } )
  } catch {
    case e: scala.MatchError => None
    case e: java.lang.ClassCastException => None
  }
}


// Fixed-point number with explicit decimal point. Example: 42.3
// object NR2 - not implemented yet


// Floating-point number in scientific notation. Example: 4.23E+02
// object NR3 - not implemented yet


/** Comprises NR1, NR2 and NR3 */
object NRf extends SCPIType[Double] {
  def apply(value: Double) = ByteCharSeq(value.toString)

  def unapply(bs: ByteCharSeq) : Option[Double] =
    try { Some(bs.toString.toDouble) } catch { case e: NumberFormatException => None }
}


object NRfSeq {
  def apply(values: Double*) = for { x <- values } yield NRf(x)

  def unapplySeq(args: Seq[ByteCharSeq]): Option[Seq[Double]] = try {
    Some( for { arg <- args} yield
      { val NRf(i) = arg.asInstanceOf[ByteCharSeq]; i } )
  } catch {
    case e: scala.MatchError => None
    case e: java.lang.ClassCastException => None
  }
}


// Extends NRf with MINimum, MAXimum, DEFault, UP, DOWN, NAN, INFinity, NINF
// object NRfp - NRF+, not implemented yet


/** Boolean Program Data. Examples: 0, 1 */
// Boolean Program Data: OFF | ON | <NRf>
// Boolean Response Data: 0 | 1

object BPD extends SCPIType[Boolean] {
  val ZERO = NR1(0)
  val ONE = NR1(1)
  val OFF = CPD(Mnemonic("OFF"))
  val ON = CPD(Mnemonic("ON"))
  
  def apply(value: Boolean) = ByteCharSeq(if (value) NR1(1) else NR1(0))

  def unapply(bs: ByteCharSeq) : Option[Boolean] = bs match {
    case ZERO => Some(false)
    case ONE => Some(true)
    case OFF => Some(false)
    case ON => Some(true)
    case _ => None
  }
}


abstract class StringData extends SCPIType[String] {
  def apply(value: String) = ByteCharSeq("\"") ++ ByteCharSeq(value) ++ ByteCharSeq("\"")

  def unapply(bs: ByteCharSeq) : Option[String] = bs match {
    case SCPIParser.sqStringExpr(contents) => Some(contents)
    case SCPIParser.dqStringExpr(contents) => Some(contents)
    case _ => None
  }
}

/** String Program Data. String enclosed in single or double quotes */
object SPD extends StringData

/** String Response Data. String enclosed in single or double quotes */
object SRD extends StringData



abstract class CharacterData {
  def apply(mnemonic: SpecMnemonic): ByteCharSeq = mnemonic.getByteCharSeq

  def unapply(bs: ByteCharSeq) : Option[RecMnemonic] =
    try { Some(RecMnemonic(bs)) } catch { case _: Throwable => None }
}

/** Character Program Data. Unquoted mnemonics in short
  * or long form */
object CPD extends CharacterData


/** Character Response Data. Unquoted mnemonics, only the short form
  * is returned. */
object CRD extends CharacterData


// Arbitrary ASCII Response Data. Undelimited 7-bit ASCII data with implied
// message terminator.
object AARD extends SCPIType[String] {
  def apply(s:String) : ByteCharSeq = ByteCharSeq(s)
  def unapply(bs: ByteCharSeq) : Option[String] = Some(bs.toString.trim)
}


/** Block Data. Binary encoded opaque block data. I. Only the definite length
  * variant (starting with #[1-9]) is implemented so far. The indefinite length
  * variant (starting with #0) must end with [CR?]+LF+EOI and is not
  * implemented yet. Only the definite length version is usable with streaming
  * protocols like RS232 or TCP/IP. */

object BlockData extends SCPIType[ByteString] {
  def apply(data: ByteString) : ByteCharSeq = {
    val tag = ByteCharSeq("#")
    val sizeStr = ByteCharSeq(data.size.toString)
    val sizeSizeStr = ByteCharSeq(sizeStr.length.toString)
    
    tag ++ sizeSizeStr ++ sizeStr ++ ByteCharSeq(data: _*)
  }

  def unapply(bs: ByteCharSeq) : Option[ByteString] = {
    val parser = SCPIParser()
    val result = parser.parseAll(parser.blockDataBytes, bs)
    if (result.successful) Some(result.get)
    else None
  }
}
