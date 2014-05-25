// Copyright (C) 2014 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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
import scala.collection.immutable.SortedMap

import daqcore.util._
import daqcore.io._
import daqcore.actors._, daqcore.actors.TypedActorTraits._

import daqcore.io.prot.keithley._, Command._
import daqcore.data.units._


trait Keithley487 {
  def rawCmd(req: ByteString): Unit
  def rawQry(req: ByteString): Future[ByteString]

  def cmd(commands: Command*): Unit
  def qryString(commands: Command*): Future[String]
  def qryResult(commands: Command*): Future[Result]
  def qryResults(commands: Command*): Future[Results]
  def qryValueDef(commands: Command*): Future[ValueDef]

  def getStatus(): Future[(String, Request, String)]

  def getOutputVoltage(): Future[Double]
  def setOutputVoltage(volt: Double, voltRng: Double, currLim: Double): Unit

  def setOutputEnabled(v: Boolean): Unit

  def getOutputVoltageStatus(): Future[(Int, Int)]

  def setInputCurrentRange(v: Double): Unit
  def runZeroCorrection(): Unit

  def getInputCurrent(): Future[Double]
  def getInputCurrentMult(nMeas: Int): Future[Seq[Double]]
}


object Keithley487 extends DeviceCompanion[Keithley487] {
  def impl = { case uri => new Keithley487Impl(uri.toString) }

  val inputCurrentRanges = SortedMap(
      0e-0 -> 0,
      2e-9 -> 1,
     20e-9 -> 2,
    200e-9 -> 3,
      2e-6 -> 4,
     20e-6 -> 5,
    200e-6 -> 6,
      2e-3 -> 7
  )

  val outputVoltageRanges = SortedMap(
      50.0 -> 0,
     500.0 -> 1
  )

  val inputCurrentLimit = SortedMap(
     25e-6 -> 0,
    2.5e-3 -> 1
  )
}


class Keithley487Impl(busURI: String) extends Keithley487
  with CloseableTAImpl
{
  import Keithley487.{inputCurrentRanges, outputVoltageRanges, inputCurrentLimit}

  import daqcore.defaults.defaultTimeout //!! get actor default timeout somehow?

  val io = ByteStreamIO(busURI, "io")
  val parser = KeithleyParser()
  val statusInfoExpr = """([0-9][0-9][0-9])(.*)c([0-9])""".r
  val voltSourceStatusExpr = """(...)(.)(.)""".r

  def rawCmd(req: ByteString) = if (!req.isEmpty) io.send(req, KeithleyStreamFramer.enc)
  def rawQry(req: ByteString) = { rawCmd(req); io.recv(KeithleyStreamFramer.dec) }

  def cmd(commands: Command*) = rawCmd(Request(commands: _*).getBytes)
  def qry(commands: Command*) = rawQry(Request(commands: _*).getBytes)
  def qryString(commands: Command*) = qry(commands: _*) map { resp => resp.decodeString("ASCII").trim }
  def qryResult(commands: Command*) = qry(commands: _*) map parser.parseResult
  def qryResults(commands: Command*) = qry(commands: _*) map parser.parseResults
  def qryValueDef(commands: Command*) = qry(commands: _*) map parser.parseValueDef

  def getStatus() = qry(U(0), X) map { resp =>
    val statusInfoExpr(model, statReqStr, lockState) = resp.decodeString("ASCII")
    val statReq = parser.parseRequest(ByteString(statReqStr))
    (model, statReq, lockState)
  }

  def getStatusMap() = getStatus() map {
    case (model, statReq, lockState) =>
      statReq.commands map { cmd => (cmd.code, cmd.params) } toMap
  }

  protected def initDevice() {
    cmd(Y(0), G(0), X) // End replies with CRLF, ASCII rdgs with prefix
    val (model, statReq, lockState) = getStatus().get
    assert (model == "487")
  }
  initDevice()

  def getOutputVoltage() = qryValueDef(U(8), X) map { case ("VS", WithUnit(v, Volt)) => v }

  def setOutputVoltage(volt: Double, voltRng: Double, currLim: Double) = {
    val vr = outputVoltageRanges.find{ case (k, _) => k >= voltRng }.get._2
    val il = inputCurrentLimit.find{ case (k, _) => k >= currLim }.get._2
    log.debug("Setting output voltage range to %s".format(vr))
    log.debug("Setting input current limit to %s".format(il))
    cmd(V(volt, vr, il), X)
  }

  def setOutputEnabled(v: Boolean) = cmd(O(if (v) 1 else 0), X);

  def getOutputVoltageStatus() = qryString(U(9), X) map {
    case voltSourceStatusExpr("487", ilim, interlock) => (ilim.toInt, interlock.toInt)
  }

  def setInputCurrentRange(v: Double) = {
    val rng = inputCurrentRanges.find{ case (k, _) => k >= v }.get._2
    log.debug("Setting input current range to %s".format(rng))
    cmd(R(rng), X)
    runZeroCorrection()
  }

  def runZeroCorrection() = cmd(C(2), X)

  def getInputCurrent() = qryResult(B(0), X) map { case Result(WithUnit(v, Ampere), DC, Result.Normal) => v }

  def getInputCurrentMult(nMeas: Int) = {
    val resp = qryResults(Q(0.01), T(4), N(nMeas), B(2), X)
    resp.map { _.results map { case Result(WithUnit(v, Ampere), DC, Result.Normal) => v } }
  }
}
