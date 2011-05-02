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


package daqcore.servers

import scala.actors._

import java.io.IOException
import java.net.InetAddress
import java.util.concurrent.TimeoutException

import akka.actor._, akka.actor.Actor._
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}

import daqcore.util._
import daqcore.actors._
import daqcore.profiles._
import daqcore.prot.snmp._

import org.snmp4j.{smi => s4jsmi, _}
import org.snmp4j.security._
import org.snmp4j.mp.SnmpConstants
import org.snmp4j.transport._
import org.snmp4j.util.DefaultPDUFactory


class SNMPv2MgrServer() extends Server /*with QueueingServer*/ with CloseableServer {
  override def profiles = super.profiles.+[SNMPv2Manager]

  val tcpTransport: Boolean = false
  val snmpVersion = SnmpConstants.version2c

  // val recvQueue = new ReplyQueue


  implicit def OID2S4J(oid: OID): s4jsmi.OID = new s4jsmi.OID(oid.values.toArray)
  implicit def S4J2OID(oid: s4jsmi.OID): OID = OID(oid.getValue.toArrayVec: _*)


  object S4JOpaqueFloat {
    // In compliance with draft-perkins-float-00, compatible with net-snmp
    // Float encoding: TAG(0x9f78) LENGTH(0x04) CONTENT(IEEE-Float-BigEndian)
    val prefix = ByteSeq(0x9f.toByte, 0x78.toByte, 0x04.toByte)

    def apply(x: Float): s4jsmi.Variable = {
      val builder = ByteSeqBuilder(7)
      builder ++= prefix
      BigEndian.putFloat(builder, x)
      new s4jsmi.Opaque(builder.result.toArray)
    }
    
    def unapply(v: s4jsmi.Variable): Option[Float] = v match {
      case op: s4jsmi.Opaque => {
        if (op.getValue.length != 7) None
        val bytes = ByteSeq.wrap(op.getValue)
        if (bytes.take(prefix.length) != prefix) None
        else Some(BigEndian.getFloat(bytes.drop(prefix.length).iterator))
      }
      case _ => None
    }
  }


  object S4JOpaqueDouble {
    // In compliance with draft-perkins-float-00, compatible with net-snmp
    // Double encoding: TAG(0x9f79) LENGTH(0x08) CONTENT(IEEE-Double-BigEndian)
    val prefix = ByteSeq(0x9f.toByte, 0x79.toByte, 0x08.toByte)

    def apply(x: Double): s4jsmi.Variable = {
      val builder = ByteSeqBuilder(11)
      builder ++= prefix
      BigEndian.putDouble(builder, x)
      new s4jsmi.Opaque(builder.result.toArray)
    }
    
    def unapply(v: s4jsmi.Variable): Option[Double] = v match {
      case op: s4jsmi.Opaque => {
        if (op.getValue.length != 11) None
        val bytes = ByteSeq.wrap(op.getValue)
        if (bytes.take(prefix.length) != prefix) None
        else Some(BigEndian.getDouble(bytes.drop(prefix.length).iterator))
      }
      case _ => None
    }
  }


  object S4JVariable {
    def apply(x: SMIValue): s4jsmi.Variable = x match {
      case Null() => new s4jsmi.Null()
      case x: OID => {val oid: s4jsmi.OID = x; oid}
      case Integer32(x) => new s4jsmi.Integer32(x)
      case Counter32(x) => new s4jsmi.Counter32(x)
      case Gauge32(x) => new s4jsmi.Gauge32(x)
      case TimeTicks(x) => new s4jsmi.TimeTicks(x)
      case Unsigned32(x) => new s4jsmi.UnsignedInteger32(x)
      case Counter64(x) => new s4jsmi.Counter64(x)
      case IpAddress(x) => new s4jsmi.IpAddress(x.toArray)
      // GenericAddress?
      // BitString?
      case OpaqueFloat(x) => S4JOpaqueFloat(x)
      case OpaqueDouble(x) => S4JOpaqueDouble(x)
      case OpaqueData(x) => new s4jsmi.Opaque(x.toArray)
      case OctetString(x) => new s4jsmi.OctetString(x.value)
    }
    
    def unapply(v: s4jsmi.Variable): Option[SMIValue] = v match {
      case x: s4jsmi.Null => Some(Null())
      case x: s4jsmi.OID => Some{val oid: OID = x; oid}
      case x: s4jsmi.Integer32 => Some(Integer32(x.getValue))
      case x: s4jsmi.Counter32 => Some(Counter32(x.getValue))
      case x: s4jsmi.Gauge32 => Some(Gauge32(x.getValue))
      case x: s4jsmi.TimeTicks => Some(TimeTicks(x.getValue))
      case x: s4jsmi.UnsignedInteger32 => Some(Unsigned32(x.getValue))
      case x: s4jsmi.Counter64 => Some(Counter64(x.getValue))
      case x: s4jsmi.IpAddress => Some(IpAddress(ByteSeq.wrap(x.toByteArray.clone)))
      // GenericAddress?
      // BitString?
      case x: s4jsmi.Opaque => Some( x match {
        case S4JOpaqueFloat(f) => OpaqueFloat(f)
        case S4JOpaqueDouble(d) => OpaqueDouble(d)
        case x => OpaqueData(ByteSeq.wrap(x.getValue.clone))
      } )
      case x: s4jsmi.OctetString => Some(x.toString)
      case x => log.warn("Unsupported SMI type: " + x.getClass); None
    }
  }


  val transport: TransportMapping = if (tcpTransport) new DefaultTcpTransportMapping() else new DefaultUdpTransportMapping()

  val snmp = new Snmp(transport)
  snmp.listen()


  def target(address: InetSockAddr, community: String): CommunityTarget = {
    val snmpAddr = transport match {
      case u: UdpTransportMapping => new s4jsmi.UdpAddress(address.getAddress, address.getPort)
      case t: TcpTransportMapping => new s4jsmi.TcpAddress(address.getAddress, address.getPort)
      case x => throw new IllegalArgumentException("Transport mapping " + x.getClass + " not supported.")
    }

    val target = new CommunityTarget()
    target.setVersion(snmpVersion)
    target.setCommunity(new s4jsmi.OctetString(community))
    target.setAddress(snmpAddr)
    target.setRetries(2)
    target.setTimeout(5000)
    target
  }


  def newPDU(pduType: Int) = {
    val pdu = DefaultPDUFactory.createPDU(snmpVersion)
    pdu.setType(pduType)
    pdu
  }


  def sendPDU(address: InetSockAddr, community: String, pdu: PDU): Seq[(OID, SMIValue)] = {
    val trg = target(address, community)
    val response = snmp.send(pdu, trg)

    response.getError match {
      case null =>
      case err => throw(err)
    }
    
    response.getResponse match {
      case null => throw new java.io.IOException("No response to SNMP PDU from " + address)
      case rpdu => for (i <- Range(0, rpdu.size)) yield {
        val vb = rpdu.get(i)
        val oid: OID = vb.getOid
        val variable = vb.getVariable
        if (variable.isException) throw new java.io.IOException("SNMP reponse contains exception: %s = %s".format(oid.toString, variable.toString))

        val S4JVariable(value) = variable
        oid -> value
      }
    }
  }

  def srvSNMPGet(address: InetSockAddr, community: String, oids: OID*) = {
    val pdu = newPDU(PDU.GET)
    for (oid <- oids) pdu.add(new s4jsmi.VariableBinding(oid))
    reply(sendPDU(address, community, pdu))
  }

  def srvSNMPGetNext(address: InetSockAddr, community: String, oids: OID*) = {
    val pdu = newPDU(PDU.GETNEXT)
    for (oid <- oids) pdu.add(new s4jsmi.VariableBinding(oid))
    reply(sendPDU(address, community, pdu))
  }

  def srvSNMPGetBulk(address: InetSockAddr, community: String, maxN: Int, oids: OID*) = {
    val pdu = newPDU(PDU.GETBULK)
    pdu.setMaxRepetitions(maxN) 
    for (oid <- oids) pdu.add(new s4jsmi.VariableBinding(oid))
    reply(sendPDU(address, community, pdu))
  }

  def srvSNMPSet(address: InetSockAddr, community: String, bindings: (OID, SMIValue)*) = {
    val pdu = newPDU(PDU.SET)
    for ((oid, value) <- bindings) pdu.add(new s4jsmi.VariableBinding(oid, S4JVariable(value)))
    reply(sendPDU(address, community, pdu))
  }

  

  
  override def init() = {
    super.init()
  }

  override def serve = super.serve orElse {
    case op @ SNMPv2Manager.SNMPGet(address, community, oids @ _*) => debug(op); srvSNMPGet(address, community, oids: _*)
    case op @ SNMPv2Manager.SNMPGetNext(address, community, oids @ _*) => debug(op); srvSNMPGetNext(address, community, oids: _*)
    case op @ SNMPv2Manager.SNMPGetBulk(address, community, n, oids @ _*) => debug(op); srvSNMPGetBulk(address, community, n, oids: _*)
    case op @ SNMPv2Manager.SNMPSet(address, community, bindings @ _*) => debug(op); srvSNMPSet(address, community, bindings: _*)
  }
}


object SNMPv2MgrServer {
  org.snmp4j.log.LogFactory.setLogFactory(new org.snmp4j.log.Log4jLogFactory)

  def apply(sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): SNMPv2Manager =
    new ServerProxy(sv.linkStart(actorOf(new SNMPv2MgrServer()), lc)) with SNMPv2Manager
}
