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


package daqcore.units


abstract class PhysUnit {
  def name: String
  def symbol: Symbol
  def quantity: Quantity
}


abstract class SIUnit extends PhysUnit


abstract class SIBaseUnit extends SIUnit {
  override def quantity: BaseQuantity
}


case object Metre extends SIBaseUnit {
  def name = "metre"
  def symbol = 'm
  def quantity = Length
}

case object Kilogram extends SIBaseUnit {
  def name = "kilogram"
  def symbol = 'kg
  def quantity = Mass
}

case object Second extends SIBaseUnit {
  def name = "second"
  def symbol = 's
  def quantity = Time
}

case object Ampere extends SIBaseUnit {
  def name = "ampere"
  def symbol = 'A
  def quantity = ECurrent
}

case object Kelvin extends SIBaseUnit {
  def name = "kelvin"
  def symbol = 'K
  def quantity = Temperature
}

case object Candela extends SIBaseUnit {
  def name = "candela"
  def symbol = 'cd
  def quantity = LIntensity
}

case object Mole extends SIBaseUnit {
  def name = "mole"
  def symbol = 'mol
  def quantity = SubstAmount
}


case object Volt extends SIUnit {
  def name = "volt"
  def symbol = 'V
  def quantity = Voltage
}

case object Ohm extends SIUnit {
  def name = "ohm"
  def symbol = 'Ohm
  def quantity = EResistance
}

