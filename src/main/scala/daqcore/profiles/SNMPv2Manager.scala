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

import akka.actor._, akka.dispatch.Future
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}

import daqcore.util._
import daqcore.actors._
import daqcore.servers._
import daqcore.prot.snmp._


trait SNMPv2Manager extends Profile with Closeable with Logging {
  import SNMPv2Manager._

  def getF(address: InetSockAddr, community: String, oids: OID*): Future[VariableBindings] =
    srv !!> SNMPGet(address, community, oids: _*)

  def getNextF(address: InetSockAddr, community: String, oids: OID*): Future[VariableBindings] =
    srv !!> SNMPGetNext(address, community, oids: _*)

  def getBulkF(address: InetSockAddr, community: String, n: Int, oids: OID*): Future[VariableBindings] =
    srv !!> SNMPGetBulk(address, community, n, oids: _*)

  def setF(address: InetSockAddr, community: String, bindings: (OID, SMIValue)*): Future[VariableBindings] =
    srv !!> SNMPSet(address, community, bindings: _*)
  
}


object SNMPv2Manager {
  case class SNMPGet(address: InetSockAddr, community: String, oids: OID*) extends ActorQuery[VariableBindings]
  case class SNMPGetNext(address: InetSockAddr, community: String, oids: OID*) extends ActorQuery[VariableBindings]
  case class SNMPGetBulk(address: InetSockAddr, community: String, n: Int, oids: OID*) extends ActorQuery[VariableBindings]
  case class SNMPSet(address: InetSockAddr, community: String, bindings: (OID, SMIValue)*) extends ActorQuery[VariableBindings]

  def apply(sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): SNMPv2Manager =
    SNMPv2MgrServer(sv, lc)
}
