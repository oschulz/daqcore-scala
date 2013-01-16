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

  def getOutVoltSensed(ch: Ch = Ch(1 to 3)): Future[ChV[Double]]
  def getOutCurrSensed(ch: Ch = Ch(1 to 3)): Future[ChV[Double]]
  def getOutPwrSensed(ch: Ch = Ch(1 to 3)): Future[ChV[Double]]
}

object RigolDP1308A extends DeviceCompanion[RigolDP1308A] {
  def impl = { case uri => new RigolDP1308AImpl(uri.toString) }
}



class RigolDP1308AImpl(ioURI: String) extends RigolDP1308A
  with CloseableTAImpl with SyncableImpl
{
  import RigolDP1308AImpl.CheckConnection

  import daqcore.defaults.defaultTimeout

  protected val nChannels = 3

  protected val codec = StringLineCodec(LineCodec.LF, "ASCII")
  protected val io = ByteStreamIO(ioURI, "io")
  def cmd(req: String): Unit = io.send(req, codec.enc)
  def qry(req: String): Future[String] = { cmd(req); io.recv(codec.dec) }
  
  protected def readChannels(req: String, ch: Ch): Future[ChV[Double]] = {
    if ((ch.min < 1) || (ch.max > nChannels)) throw new IllegalArgumentException("Invalid channel number")
    Future.sequence(
      for (i <- ch.toSeq) yield {
        cmd("INST:NSEL %s".format(i))
        val selInst = qry("INST:NSEL?").get.toInt
        if (selInst != i) throw new RuntimeException("Channel selection failed")
        qry(req) map { v => (i, v.toDouble) }
      }
    ) map { xs => ChV(xs: _*) }
  }

  protected def getIdentity() = qry("*IDN?").get
  protected val idn = getIdentity()
  def identity = successful(idn)
  
  // Device specific workaround: Native VXI-11 connection becomes stale if idle for too long
  protected def checkConnection() {
    assert( getIdentity() == idn )
    scheduleOnce(10.seconds, selfRef, CheckConnection)
    log.trace("Connection checked")
  }

  checkConnection()

  def getOutVoltSensed(ch: Ch = Ch(1 to 3)) = readChannels("MEAS:VOLT?", ch)
  def getOutCurrSensed(ch: Ch = Ch(1 to 3)) = readChannels("MEAS:CURR?", ch)
  def getOutPwrSensed(ch: Ch = Ch(1 to 3)) = readChannels("MEAS:POWE?", ch)

  override def receive = extend(super.receive) {
    case CheckConnection => checkConnection()
  }
}


object RigolDP1308AImpl {
  protected case object CheckConnection
}
