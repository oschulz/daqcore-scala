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
import daqcore.data.units._


abstract class MeasFunc
case object VAL extends MeasFunc
case object DC extends MeasFunc
case object AC extends MeasFunc


// Measurement Result: Function (e.g. DC, AC, PEAK, ...) of a quantity (e.g. Voltage)

case class Result (value: WithUnit, func: MeasFunc, tp: Result.Type)

object Result {
	abstract class Type
	case object Normal extends Type
	case object Relative extends Type
	case object Overflow extends Type
}


case class Results(results: Result*)
