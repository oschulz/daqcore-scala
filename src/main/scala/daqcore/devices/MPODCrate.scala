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


package daqcore.devices

import akka.actor.Actor.actorOf, akka.actor.ActorRef, akka.dispatch.Future

import daqcore.util._
import daqcore.actors._
import daqcore.io._
import daqcore.io.prot.snmp._
import akka.dispatch.Future

import collection.immutable.Queue


trait MPODCrate extends PowerSupply

object MPODCrate {
  def apply(aref: ActorRef) = SReqProxy[MPODCrate](aref)
  
  def apply(address: InetSockAddr, sv: Supervising = defaultSupervisor): MPODCrate =
    this(sv.linkStart(new MPODCrateSrv(address)))
}



class MPODCrateSrv(address: InetSockAddr) extends MServer {
  val snmp = defaultSnmpManager
  
  val maxPDUsPerRequest = 32
  
  val getcom = "public"
  val setcom = "guru"

  def snmpGet(oids: OID*): VariableBindings =
    (oids.grouped(maxPDUsPerRequest) flatMap {oids => snmp.getF(address, setcom, oids: _*).get}).toList

  def snmpGetNext(oids: OID*): VariableBindings =
    (oids.grouped(maxPDUsPerRequest) flatMap {oids => snmp.getNextF(address, setcom, oids: _*).get}).toList

  def snmpGetBulk(n: Int, oids: OID*): VariableBindings = {
    def snmpGetBulkImpl(n: Int, oids: OID*)(prev: Queue[VariableBindings]): VariableBindings = {
      val nOIDs = oids.length
      val resp = snmp.getBulkF(address, setcom, n, oids: _*).get
      val allResp = prev :+ resp
      if ((resp.length >= nOIDs) && (resp.size < n * nOIDs) && (resp.size % nOIDs == 0)) {
        val newOIDS = resp.takeRight(nOIDs) map {_._1}
        snmpGetBulkImpl(n - (resp.size / nOIDs), newOIDS: _*)(allResp)
      } else allResp.flatten.toList
    }
    snmpGetBulkImpl(n, oids: _*)(Queue.empty[VariableBindings])
  }

  def snmpGetChildren(oids: OID*): VariableBindings = {
    def snmpGetChildrenImpl(oids: OID*)(parents: Seq[OID], prev: Queue[VariableBindings]): VariableBindings = {
      val nOIDs = oids.length
      
      val resp: VariableBindings = ( for {
        row <- snmp.getBulkF(address, setcom, 100, oids: _*).get.grouped(nOIDs)
        if row.corresponds(parents) { _._1 isChildOf _ }
        vb <- row
      } yield {vb} ) toList
      
      assert { resp.length % nOIDs == 0 }
      
      val allResp = prev :+ resp

      if (resp.size >= nOIDs) {
        val newOIDS = resp.takeRight(nOIDs) map {_._1}
        snmpGetChildrenImpl(newOIDS: _*)(parents, allResp)
      } else allResp.flatten.toList
    }
    snmpGetChildrenImpl(oids: _*)(oids, Queue.empty[VariableBindings])
  }

  def snmpSet(bindings: (OID, SMIValue)*): VariableBindings =
    (bindings.grouped(maxPDUsPerRequest) flatMap {bindings => snmp.setF(address, setcom, bindings: _*).get}).toList

  object oids {
    val snmpv2MIB = MIB(this.getClass.getResource("/mibs/ietf/SNMPv2-MIB"))
    val sysDescr = snmpv2MIB.symbols("sysDescr").oid
    val sysUpTime = snmpv2MIB.symbols("sysUpTime").oid
    val sysLocation = snmpv2MIB.symbols("sysLocation").oid

    val ifMIB = MIB(this.getClass.getResource("/mibs/ietf/IF-MIB"))
    val ifPhysAddress = ifMIB.symbols("ifPhysAddress").oid

    val wienerMIB = MIB(this.getClass.getResource("/mibs/private/WIENER-CRATE-MIB.txt"))
    val outputTable = wienerMIB.symbols("outputTable").oid
    val outputSwitch = wienerMIB.symbols("outputSwitch").oid
    val outputVoltage = wienerMIB.symbols("outputVoltage").oid
    val outputMeasurementSenseVoltage = wienerMIB.symbols("outputMeasurementSenseVoltage").oid
    val outputIndex = wienerMIB.symbols("outputIndex").oid
    val outputName = wienerMIB.symbols("outputName").oid
    val outputGroup = wienerMIB.symbols("outputGroup").oid
    val outputVoltageRiseRate = wienerMIB.symbols("outputVoltageRiseRate").oid
    val outputVoltageFallRate = wienerMIB.symbols("outputVoltageFallRate").oid
  }
  
  def colIntVals(bindings: VariableBindings): Seq[(Int, Int)] = for { (oid,v) <- bindings } yield { val ch = oid.values.last; val Integer32(x) = v; (ch, x) }
  def getIntCol(chOID: OID): Seq[(Int, Int)] = colIntVals(snmpGetChildren(chOID))
  def setIntCol(chOID: OID, vals: (Int, Int)*): Seq[(Int, Int)] = {
    val bindings = for { (ch, x) <- vals } yield (chOID~ch, Integer32(x.toInt))
    colIntVals(snmpSet(bindings: _*))
  }

  def colBoolIntVals(bindings: VariableBindings): Seq[(Int, Boolean)] = for { (oid,v) <- bindings } yield { val ch = oid.values.last; val Integer32(x) = v; (ch, if (x>0) true else false) }
  def getBoolIntCol(chOID: OID): Seq[(Int, Boolean)] = colBoolIntVals(snmpGetChildren(chOID))
  def setBoolIntCol(chOID: OID, vals: (Int, Boolean)*): Seq[(Int, Boolean)] = {
    val bindings = for { (ch, x) <- vals } yield (chOID~ch, Integer32(if (x) 1 else 0))
    colBoolIntVals(snmpSet(bindings: _*))
  }

  def colFloatVals(bindings: VariableBindings): Seq[(Int, Double)] = for { (oid,v) <- bindings } yield { val ch = oid.values.last; val OpaqueFloat(x) = v; (ch, x.toDouble) }
  def getFloatCol(chOID: OID): Seq[(Int, Double)] = colFloatVals(snmpGetChildren(chOID))
  def setFloatCol(chOID: OID, vals: (Int, Double)*): Seq[(Int, Double)] = {
    val bindings = for { (ch, x) <- vals } yield (chOID~ch, OpaqueFloat(x.toFloat))
    colFloatVals(snmpSet(bindings: _*))
  }
  
  @sreq val identity: String = {val OctetString(res) = snmpGetNext(oids.sysDescr).head._2; res}
  
  @sreq val outputs: Seq[Int] = snmpGetChildren(oids.outputIndex) map { _._1.values.last }


  @sreq def getOutEnabled(): Seq[(Int, Boolean)] = getBoolIntCol(oids.outputSwitch)
  @sreq def setOutEnabled(vals: (Int, Boolean)*) = setBoolIntCol(oids.outputSwitch, vals: _*)

  @sreq def getOutVoltDesired(): Seq[(Int, Double)] = getFloatCol(oids.outputVoltage)
  @sreq def setOutVoltDesired(vals: (Int, Double)*) = setFloatCol(oids.outputVoltage, vals: _*)

  @sreq def getOutVoltRiseRate(): Seq[(Int, Double)] = getFloatCol(oids.outputVoltageRiseRate)
  @sreq def setOutVoltRiseRate(vals: (Int, Double)*) = setFloatCol(oids.outputVoltageRiseRate, vals: _*)

  @sreq def getOutVoltFallRate(): Seq[(Int, Double)] = getFloatCol(oids.outputVoltageFallRate)
  @sreq def setOutVoltFallRate(vals: (Int, Double)*) = setFloatCol(oids.outputVoltageFallRate, vals: _*)


  @sreq def getOutVoltSensed(): Seq[(Int, Double)] = getFloatCol(oids.outputMeasurementSenseVoltage)
}
