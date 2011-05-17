// Copyright (C) 2011 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.profiles

import akka.actor.Actor.actorOf, akka.actor.ActorRef, akka.dispatch.Future

import daqcore.util._
import daqcore.actors._
import daqcore.servers._


trait MPODCrate extends Profile {
  def sysDescr() = srv.qryF[String]('sysDescr)
  
  def outputs() = srv.qryF[Seq[Int]]('outputs)

  def getOutEnabled() = srv.qryF[Seq[(Int, Boolean)]]('getOutEnabled)
  def setOutEnabled(vals: (Int, Boolean)*) = srv.qryF[Seq[(Int, Boolean)]]('setOutEnabled, vals)

  def getOutVoltDesired() =  srv.qryF[Seq[(Int, Double)]]('getOutVoltDesired)
  def setOutVoltDesired(vals: (Int, Double)*) =  srv.qryF[Seq[(Int, Double)]]('setOutVoltDesired, vals)

  def getOutVoltSensed() = srv.qryF[Seq[(Int, Double)]]('getOutVoltSensed)

  def getOutVoltRiseRate() =  srv.qryF[Seq[(Int, Double)]]('getOutVoltRiseRate)
  def setOutVoltRiseRate(vals: (Int, Double)*) =  srv.qryF[Seq[(Int, Double)]]('setOutVoltRiseRate, vals)

  def getOutVoltFallRate() =  srv.qryF[Seq[(Int, Double)]]('getOutVoltFallRate)
  def setOutVoltFallRate(vals: (Int, Double)*) =  srv.qryF[Seq[(Int, Double)]]('setOutVoltFallRate, vals)
}

object MPODCrate {
  def apply(address: InetSockAddr, sv: Supervising = defaultSupervisor): MPODCrate =
    new ServerProxy(sv.linkStart(actorOf(new MPODCrateSrv(address)))) with MPODCrate
}
