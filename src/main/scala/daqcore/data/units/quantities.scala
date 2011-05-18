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


package daqcore.data.units


abstract class Quantity {
  def name: String
  def symbol: Symbol
}

abstract class BaseQuantity extends Quantity

case object Length extends BaseQuantity {
  def name = "length"
  def symbol = 'l
}

case object Mass extends BaseQuantity {
  def name = "mass"
  def symbol = 'm
}

case object Time extends BaseQuantity {
  def name = "time"
  def symbol = 't
}

case object ECurrent extends BaseQuantity {
  def name = "electric current"
  def symbol = 'I
}

case object Temperature extends BaseQuantity {
  def name = "thermodynamic temperature"
  def symbol = 'T
}

case object LIntensity extends BaseQuantity {
  def name = "luminous intensity"
  def symbol = 'I_v
}

case object SubstAmount extends BaseQuantity {
  def name = "amount of substance"
  def symbol = 'n
}


case object Voltage extends Quantity {
  def name = "voltage"
  def symbol = 'n
}

case object EResistance extends Quantity {
  def name = "electric resistance"
  def symbol = 'R
}
