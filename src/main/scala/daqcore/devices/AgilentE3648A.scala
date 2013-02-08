// Copyright (C) 2013 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.concurrent.{Future, Promise}
import akka.actor._

import daqcore.actors._
import daqcore.util._
import daqcore.io._
import daqcore.io.prot.scpi._, daqcore.io.prot.scpi.mnemonics._


trait AgilentE3648A extends SCPIDevice {
  def getOutEnabled(ch: Ch = Ch(1 to 2)): Future[ChV[Boolean]]
  def setOutEnabled(vals: ChV[Boolean]): Unit

  def getOutVoltLimit(ch: Ch = Ch(1 to 2)): Future[ChV[Double]]
  def setOutVoltLimit(vals: ChV[Double]): Unit

  def getOutCurrLimit(ch: Ch = Ch(1 to 2)): Future[ChV[Double]]
  def setOutCurrLimit(vals: ChV[Double]): Unit

  def getOutVoltSensed(ch: Ch = Ch(1 to 2)): Future[ChV[Double]]
  def getOutCurrSensed(ch: Ch = Ch(1 to 2)): Future[ChV[Double]]

  def setOutVoltRange(vals: ChV[Double]): Unit
  def getOutVoltRange(ch: Ch = Ch(1 to 2)): Future[ChV[Double]]
}

object AgilentE3648A extends DeviceCompanion[AgilentE3648A] {
  def impl = { case uri => new AgilentE3648AImpl(uri.toString) }

  object VoltageRangeSpec extends SCPIType[Double] {
    private val voltageRanges: Map[ByteCharSeq, Double] = Map("P8V" -> 8.0, "P20V" -> 20.0, "P35V" -> 35.0, "P60V" -> 60.0) map { case (k,v) => (ByteCharSeq(k), v)}
    private val voltageRangesInv: Seq[(Double, ByteCharSeq)] = voltageRanges.toSeq map {_.swap} sortWith { _._1 < _._1 }

    def apply(x: Double) = voltageRangesInv.find(_._1 >= x) map (_._2) getOrElse ByteCharSeq("P60V")

    def unapply(bs: ByteCharSeq) : Option[Double] = voltageRanges.get(bs)
  }
}



class AgilentE3648AImpl(ioURI: String) extends AgilentE3648A
  with SCPICompliantDeviceImpl
{
  import daqcore.defaults.defaultTimeout //!! get actor default timeout somehow?

  import AgilentE3648A.VoltageRangeSpec

  val io = ByteStreamIO(ioURI, "io")
  
  protected val nChannels = 2

  protected def nsel(ch: Int) = {
    if ((ch >= 1) && (ch <= nChannels)) ~INSTrument~NSELect!(ch)
    else throw new IllegalArgumentException("Channel %s does not exist".format(ch))
  }

  def setValue[A](header: Header, scpiType: SCPIType[A], vals: ChV[A]): Unit =
    cmd(vals.toSeq flatMap { case (ch, v) => Seq(nsel(ch), header!(scpiType(v))) } :_*)
  
  def getValue[A](header: Header, scpiType: SCPIType[A], ch: Ch): Future[ChV[A]] = {
    val channels = ch.toSeq
    val resp = qry( channels flatMap { ch => Seq(nsel(ch), header?) } :_*).get
    val results: Seq[A] = resp.results map { case Result(r) => r match { case scpiType(v) => v} }
    successful( ChV(channels zip results : _*) )
  }
  
  def getOutEnabled(ch: Ch = Ch(1 to 2)) = getValue(~OUTPut~STATe, BPD, ch)
  def setOutEnabled(vals: ChV[Boolean]) = setValue(~OUTPut~STATe, BPD, vals)

  def getOutVoltLimit(ch: Ch = Ch(1 to 2)) = getValue(~SOURce~VOLTage, NRf, ch)
  def setOutVoltLimit(vals: ChV[Double]) = setValue(~SOURce~VOLTage, NRf, vals)

  def getOutCurrLimit(ch: Ch = Ch(1 to 2)) = getValue(~SOURce~CURRent, NRf, ch)
  def setOutCurrLimit(vals: ChV[Double]) = setValue(~SOURce~CURRent, NRf, vals)

  def getOutVoltSensed(ch: Ch = Ch(1 to 2)) = getValue(~MEASure~VOLTage, NRf, ch)
  def getOutCurrSensed(ch: Ch = Ch(1 to 2)) = getValue(~MEASure~CURRent, NRf, ch)

  def setOutVoltRange(vals: ChV[Double]) = setValue(~VOLTage~RANGe, VoltageRangeSpec, vals);
  def getOutVoltRange(ch: Ch = Ch(1 to 2)) = getValue(~VOLTage~RANGe, VoltageRangeSpec, ch);
}
