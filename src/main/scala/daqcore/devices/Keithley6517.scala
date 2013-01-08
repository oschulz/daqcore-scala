// Copyright (C) 2011-2013 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.language.postfixOps

import akka.actor._
import scala.concurrent.{Future, Promise}

import daqcore.util._
import daqcore.io._
import daqcore.actors._, daqcore.actors.TypedActorTraits._

import daqcore.io.prot.scpi._, daqcore.io.prot.scpi.mnemonics._

import collection.immutable.Queue


object Keithley6517 {
  val currentReadingFmt = """(.*)(.)ADC,.*,.*rdng#.*""".r
  val voltageReadingFmt = """(.*)(.)VDC,.*,.*rdng#.*""".r

  val ZCHeck = Mnemonic("ZCHeck")
  val SYSTem = Mnemonic("SYSTem")
  val SENSe = Mnemonic("SENSe")
}



trait Keithley6517Current extends Device {
  def cmd(instr: Instruction*): Unit
  def qry(instr: Instruction*): Future[Response]

  def rawCmd(req: ByteString): Unit
  def rawQry(req: ByteString): Future[ByteString]

  def reset(): Future[Unit]

  def getInputZeroCheck(): Future[Boolean]
  def setInputZeroCheck(v: Boolean): Future[Boolean]

  def getInputCurrRange(): Future[Double]
  def setInputCurrRange(v: Double): Future[Double]

  def getInputCurrData(): Future[(Double, String)]
  def getInputCurrSensed(): Future[Double]
  def getInputCurrState(): Future[String]
}


object Keithley6517Current extends DeviceCompanion[Keithley6517Current] {
  def impl = { case uri => new Keithley6517CurrentImpl(uri.toString) }
}


// Currently *must* be run over a VXI-11 GPIB connection, instrument
// behaves differently on a serial interface, also SCPI stream decoding
// is currently unavailable due to "#" characters in instrument
// responses

class Keithley6517CurrentImpl(busURI: String) extends Keithley6517Current
  with CloseableTAImpl with SyncableImpl
{
  import Keithley6517._

  import daqcore.defaults.defaultTimeout //!! get actor default timeout somehow?

  val io = ByteStreamIO(busURI)
  val msgIO = SCPIStreamFramer(io)
  val parser = new SCPIParser

  def respNRfDouble: PartialFunction[Response, Double] = _ match { case Response(Result(NRf(r))) => r }
  def respNR1Int: PartialFunction[Response, Int] = _ match { case Response(Result(NR1(r))) => r }
  def respNR1Boolean: PartialFunction[Response, Boolean] = _ match { case Response(Result(NR1(r))) => if (r>0) true else false }
  def respNR1One: PartialFunction[Response, Unit] = _ match { case Response(Result(NR1(1))) => {} }
  def respNR1Zero: PartialFunction[Response, Unit] = _ match { case Response(Result(NR1(0))) => {} }
  
  def cmd(instr: Instruction*) = rawCmd(Request(instr: _*).getBytes)
  def qry(instr: Instruction*) = rawQry(Request(instr: _*).getBytes) map parser.parseResponse

  def rawCmd(req: ByteString) = msgIO.send(req)
  // Can't use msgIO.recv due to "#" characters in instrument responses:
  def rawQry(req: ByteString) = { msgIO.send(req); io.recv() } 

  
  protected val deviceIDN = qry(IDN?) map { case Response(Result(AARD(idn))) => idn } get
  def identity() = successful(deviceIDN)

  protected def initDevice() {
    qry(~ABORt!, ESR?).get
    assert { (qry(~SYSTem~ZCHeck!(1), ~CONFigure~CURRent~DC!, ESR?) map respNR1Int get) == 0 }
  }
  initDevice()
  
  override def sync() = cmd(WAI!)
  override def getSync() = qry(OPC?) map respNR1One

  def reset() = qry(RST!, OPC?) map respNR1One

  def getInputZeroCheck(): Future[Boolean] = qry(~SYSTem~ZCHeck?) map respNR1Boolean
  def setInputZeroCheck(v: Boolean) = qry(~SYSTem~ZCHeck!(if (v) 1 else 0), ~SYSTem~ZCHeck?) map respNR1Boolean

  def getInputCurrRange() = qry(~SENSe~CURRent~RANGe?) map respNRfDouble
  def setInputCurrRange(v: Double) = qry(~SENSe~CURRent~RANGe!(v), ~SENSe~CURRent~RANGe?) map respNRfDouble

  def getInputCurrData() = qry(~READ?) map { case Response(Result(AARD(currentReadingFmt(v, s)))) => (v.toDouble, s) }
  def getInputCurrSensed() = getInputCurrData() map { case (v,s) => v}
  def getInputCurrState() = getInputCurrData() map { case (v,s) => s}
}
