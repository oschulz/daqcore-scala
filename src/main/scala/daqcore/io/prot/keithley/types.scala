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


package daqcore.io.prot.keithley

import daqcore.util._


object IntVal {
  def apply(value: Int) = ByteCharSeq(value.toString)

  def unapply(bs: ByteCharSeq) : Option[Int] =
    try { Some(bs.toString.toInt) } catch { case e: NumberFormatException => None }
}


object FPVal {
  def apply(value: Double) = ByteCharSeq(value.toString)

  def unapply(bs: ByteCharSeq) : Option[Double] =
    try { Some(bs.toString.toDouble) } catch { case e: NumberFormatException => None }
}


object Chars {
  val charsExpr = """([\w\s]*)""".r

  def apply(value: String) = {
    require( value match { case charsExpr(s) => true; case _ => false } )
    ByteCharSeq(value)
  }

  def unapply(bs: ByteCharSeq) : Option[String] = Some(bs.toString)
}

/*object Normal {
  def unapply(bs: ByteCharSeq) : Option[ByteCharSeq] =
    if (bs.startsWith(ByteCharSeq('N'))) Some(bs.subSequence(1, bs.length))
    else None
}

object Resistance {
}*/

/*
Measurement result format:

NDCV-l234567E+O

(N | O) (OHM | ((DC|AC)(V|I))) <double>
N: Normal Reading
O: Overflow
OHM: Resistance measurement
DC: direct current/voltage
AC: alternating current/voltage
V: Voltage measurement
I: Current measurement
<double> Result in SI-Units

Examples:
OOHM+9.999999E+9
NDCV+002.2131E+0
NDCI+0.00003E-09

*/
