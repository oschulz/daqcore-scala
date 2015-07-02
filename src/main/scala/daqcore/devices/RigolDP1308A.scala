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

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import akka.actor._

import daqcore.actors._
import daqcore.util._
import daqcore.io._


trait RigolDP1308A extends Device {
  def cmd(req: String): Unit
  def qry(req: String): Future[String]

  def getOutEnabled(ch: Ch = Ch(1 to 3)): Future[ChV[Boolean]]
  def setOutEnabled(vals: ChV[Boolean]): Unit

  def getOutVoltDesired(ch: Ch = Ch(1 to 3)): Future[ChV[Double]]
  def setOutVoltDesired(vals: ChV[Double]): Unit

  def getOutVoltSensed(ch: Ch = Ch(1 to 3)): Future[ChV[Double]]
  def getOutCurrSensed(ch: Ch = Ch(1 to 3)): Future[ChV[Double]]
  def getOutPwrSensed(ch: Ch = Ch(1 to 3)): Future[ChV[Double]]
}

object RigolDP1308A extends DeviceCompanion[RigolDP1308A] {
  def impl = { case uri => new RigolDP1308AImpl(uri.toString) }
}



class RigolDP1308AImpl(ioURI: String) extends RigolDP1308A
  with CloseableTAImpl with SyncableImpl with DeviceKeepAlive
{
  // Note / Workaround: *Must* mix in DeviceKeepAlive, because native VXI-11
  // connection to device ethernet port seems to become stale if idle for
  // too long.

  import daqcore.defaults.defaultTimeout
  implicit def executor = defaultExecContext

  val chName = ChV(1 -> "P6V", 2 -> "P25V", 3 -> "N25V")
  protected val nChannels = chName.size

  protected val codec = StringLineCodec(LineCodec.LF, "ASCII")
  protected val io = ByteStreamIO(ioURI, "io")
  def cmd(req: String): Unit = io.send(req, codec.enc)
  def qry(req: String): Future[String] = { cmd(req); io.recv(codec.dec) }
  
  protected def getCh[A](req: String, ch: Ch)(f: String => A): Future[ChV[A]] = {
    Future.sequence(
      for (i <- ch.toSeq) yield
        qry("%s? %s".format(req, chName(i))) map { v => (i, f(v)) }
    ) map { xs => ChV(xs: _*) }
  }

  protected def setCh[A](req: String, vals: ChV[A])(f: A => String): Unit = {
    for ((i, v) <- vals) cmd("%s %s,%s".format(req, chName(i), f(v)))
  }
  
  protected def getIdentity() = qry("*IDN?").get


  def getOutEnabled(ch: Ch = Ch(1 to 3)) = getCh("OUTP:STAT", ch){_ match { case "ON" => true; case "OFF" => false}};
  def setOutEnabled(vals: ChV[Boolean]) = setCh("OUTP:STAT", vals){ v => if (v) "ON" else "OFF" }

  def getOutVoltDesired(ch: Ch = Ch(1 to 3)) = ??? //!! TODO: Implement
  def setOutVoltDesired(vals: ChV[Double]) = ??? //!! TODO: Implement

  def getOutVoltSensed(ch: Ch = Ch(1 to 3)) = getCh("MEAS:VOLT", ch){_.toDouble}
  def getOutCurrSensed(ch: Ch = Ch(1 to 3)) = getCh("MEAS:CURR", ch){_.toDouble}
  def getOutPwrSensed(ch: Ch = Ch(1 to 3)) = getCh("MEAS:POWE", ch){_.toDouble}
}
