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
  
  def fpNumber: Parser[Double] = fpNumberExp ^^
    { bs => val FPVal(x) = bs; x}

  def dcv: Parser[(WithUnit,MeasFunc)] =
    lit("DCV") ~> fpNumber ^^ { v => (WithUnit(v, Volt), DC) }

  def acv: Parser[(WithUnit,MeasFunc)] =
    lit("ACV") ~> fpNumber ^^ { v => (WithUnit(v, Volt), AC) }

  def dci: Parser[(WithUnit,MeasFunc)] =
    lit("DCI") ~> fpNumber ^^ { v => (WithUnit(v, Ampere), DC) }

  def aci: Parser[(WithUnit,MeasFunc)] =
    lit("ACI") ~> fpNumber ^^ { v => (WithUnit(v, Ampere), AC) }

  def ohm: Parser[(WithUnit,MeasFunc)] =
    lit("OHM") ~> fpNumber ^^ { v => (WithUnit(v, Ohm), VAL) }
  
  def funcWithValue = dcv | acv | dci | aci | ohm
  
  def normal: Parser[Result] =
    lit("N") ~> funcWithValue ^^ { fv => Normal(fv._1, fv._2)}

  def overflow: Parser[Result] =
    lit("O") ~> funcWithValue ^^ { fv => Overflow(fv._1, fv._2)}
  
  def result: Parser[Result] = normal | overflow


  def parseResult(in: java.lang.CharSequence): Result =
    parseAll(result, in).get
}


object KeithleyParser {
  val measFuncExp = """[A-Z][A-Z][A-Z]""".r
  val fpNumberExp = """[-+]?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r
  
  protected val tlParser = new ThreadLocal[KeithleyParser]

  def parser = {
    if (tlParser.get == null) tlParser.set(new KeithleyParser)
    tlParser.get
  }

  def apply() = parser
}
