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


package daqcore.prot.keithley


import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.matching.Regex

import daqcore.util._
import daqcore.units._


class KeithleyParser extends ByteCharSeqParsers {
  import KeithleyParser._
  
  lazy val fpNumber: PackratParser[Double] = fpNumberExp ^^
    { bs => val FPVal(x) = bs; x}

  lazy val dcv: PackratParser[(WithUnit,MeasFunc)] =
    lit("DCV") ~> fpNumber ^^ { v => (WithUnit(v, Volt), DC) }

  lazy val acv: PackratParser[(WithUnit,MeasFunc)] =
    lit("ACV") ~> fpNumber ^^ { v => (WithUnit(v, Volt), AC) }

  lazy val dci: PackratParser[(WithUnit,MeasFunc)] =
    lit("DCI") ~> fpNumber ^^ { v => (WithUnit(v, Ampere), DC) }

  lazy val aci: PackratParser[(WithUnit,MeasFunc)] =
    lit("ACI") ~> fpNumber ^^ { v => (WithUnit(v, Ampere), AC) }

  lazy val ohm: PackratParser[(WithUnit,MeasFunc)] =
    lit("OHM") ~> fpNumber ^^ { v => (WithUnit(v, Ohm), VAL) }
  
  lazy val funcWithValue = dcv | acv | dci | aci | ohm
  
  lazy val normal: PackratParser[Result] =
    lit("N") ~> funcWithValue ^^ { fv => Normal(fv._1, fv._2)}

  lazy val overflow: PackratParser[Result] =
    lit("O") ~> funcWithValue ^^ { fv => Overflow(fv._1, fv._2)}
  
  lazy val result: PackratParser[Result] = normal | overflow


  def parseResult(in: java.lang.CharSequence): Result =
    parseAll(result, in).get
}


object KeithleyParser {
  val measFuncExp = """[A-Z][A-Z][A-Z]""".r
  val fpNumberExp = """[-+]?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r
  
  protected lazy val tlParser =
    { val p = new ThreadLocal[KeithleyParser];  p set (new KeithleyParser); p }

  def parser = tlParser.get

  def apply() = parser
}
