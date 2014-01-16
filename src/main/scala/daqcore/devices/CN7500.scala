// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import akka.actor._

import daqcore.actors._, daqcore.actors.TypedActorTraits._
import daqcore.util._
import daqcore.io._
import daqcore.io.prot.modbus._
import ModbusMsg.{RawReq, RawResp}

import collection.immutable.Queue


trait CN7500 extends Device {
  def readRegister(address: Int): Future[Short]
  def readRegisters(address: Int, count: Int): Future[ArrayVec[Short]]
  def writeRegister(address: Int, value: Short): Future[Short]

  def readBit(address: Int): Future[Boolean]
  def readBits(address: Int, count: Int): Future[ArrayVec[Boolean]]
  def writeBit(address: Int, value: Boolean): Future[Boolean]
  
  def getTempPV(): Future[Double]
  def getTempSP(): Future[Double]
  def setTempSP(value: Double): Future[Double]

  def getRunning(): Future[Boolean]
  def setRunning(value: Boolean): Future[Boolean]
  
  def getAutoTuning(): Future[Boolean]
  def setAutoTuning(value: Boolean): Future[Boolean]

  def getOutputLevel(ch: Ch = Ch(1 to 2)): Future[ChV[Double]]
}

object CN7500 {
  def apply(aref: ActorRef)(implicit sys: ActorSystem) = typedActor[CN7500](aref)

  def apply(busURI: String, slave: Int, name: String)(implicit rf: ActorRefFactory): CN7500 =
    typedActorOf[CN7500](new CN7500Impl(busURI, slave), name)

  def apply(bus: ModbusServer, slave: Int, name: String = "")(implicit rf: ActorRefFactory): CN7500 =
    apply(actorRef(bus).path.toString, slave, name)(rf)
}



class CN7500Impl(busURI: String, slave: Int) extends CN7500
  with CloseableTAImpl with SyncableImpl
{
  import CN7500Impl._

  import daqcore.defaults.defaultTimeout //!! get actor default timeout somehow?
  
  val bus = typedActor[ModbusServer](findActor(busURI).get.get)
  
  def reg2temp(x: Short): Double = (x.toInt & 0xffff) * 0.1
  def temp2reg(x: Double): Short = ((x / 0.1).toInt & 0xffff).toShort
  
  def identity = successful("CN7500")

  protected def checkConnection() {
    assert( bus.query(RawReq(slave, 0x67, ByteString(0x00, 0x12, 0x00, 0x03))).get ==
      RawResp(slave, 0x67, ByteString(0x06, 0x00, 0x02, 0x00, 0x01, 0x00, 0x00)) )

    scheduleOnce(30.seconds, selfRef, CheckConnection)
    log.trace("Connection checked")
  }

  checkConnection()


  def readRegister(address: Int) = bus.readRegister(slave, address)
  def readRegisters(address: Int, count: Int) = bus.readRegisters(slave, address, count)
  def writeRegister(address: Int, value: Short) = bus.writeRegister(slave, address, value)

  def readBit(address: Int) = bus.readBitInput(slave, address)
  def readBits(address: Int, count: Int) = bus.readBitInputs(slave, address, count)
  def writeBit(address: Int, value: Boolean) = bus.writeBit(slave, address, value)


  def getTempPV() = readRegister(0x1000) map reg2temp
  def getTempSP() = readRegister(0x1001) map reg2temp
  def setTempSP(value: Double) = writeRegister(0x1001, temp2reg(value)) map reg2temp

  def getRunning() = readBit(0x814)
  def setRunning(value: Boolean) = writeBit(0x814, value)
  
  def getAutoTuning() = readBit(0x813)
  def setAutoTuning(value: Boolean) = writeBit(0x813, value)

  def getOutputLevel(ch: Ch) =
    readRegisters(0x1012,2) map { vals => ChV(vals){ case (i,v) if (ch contains i+1) => (i+1, reg2temp(v))} }

  override def receive = extend(super.receive) {
    case CheckConnection => checkConnection()
  }
}


object CN7500Impl {
  protected case object CheckConnection
}
