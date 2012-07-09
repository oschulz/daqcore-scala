// Copyright (C) 2011-2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.devices

import daqcore.actors._
import akka.dispatch.Future


trait PowerSupply extends Device {
  def outputs(): Future[Seq[Int]]

  def getOutEnabled(channels: Int*): Future[Seq[(Int, Boolean)]]
  def setOutEnabled(vals: (Int, Boolean)*): Future[Seq[(Int, Boolean)]]

  def getOutVoltDesired(channels: Int*): Future[Seq[(Int, Double)]]
  def setOutVoltDesired(vals: (Int, Double)*): Future[Seq[(Int, Double)]]

  def getOutVoltSensed(channels: Int*): Future[Seq[(Int, Double)]]
  def getOutCurrSensed(channels: Int*): Future[Seq[(Int, Double)]]

  def getOutVoltRiseRate(channels: Int*): Future[Seq[(Int, Double)]]
  def setOutVoltRiseRate(vals: (Int, Double)*): Future[Seq[(Int, Double)]]

  def getOutVoltFallRate(channels: Int*): Future[Seq[(Int, Double)]]
  def setOutVoltFallRate(vals: (Int, Double)*): Future[Seq[(Int, Double)]]

  def getOutCurrTrip(channels: Int*): Future[Seq[(Int, Double)]]
  def setOutCurrTrip(vals: (Int, Double)*): Future[Seq[(Int, Double)]]
}
