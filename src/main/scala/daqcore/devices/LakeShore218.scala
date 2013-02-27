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
import akka.actor._

import daqcore.actors._
import daqcore.util._
import daqcore.io._


//!! TODO: Change to SCPIDevice
trait LakeShore218 extends Device {
  def readTemp(ch: Ch = Ch(1 to 8)): Future[ChV[Double]] 
}

object LakeShore218 extends DeviceCompanion[LakeShore218] {
  def impl = { case uri => new Lakeshore218(uri.toString) }
}



class Lakeshore218(ioURI: String) extends LakeShore218
  with CloseableTAImpl with SyncableImpl
{
  import daqcore.defaults.defaultTimeout //!! get actor default timeout somehow?

  import AgilentE3648A.VoltageRangeSpec

  import daqcore.io.prot.modbus.BigEndianShortCoding._

  protected val nChannels = 8

  protected val codec = StringLineCodec(LineCodec.CRLF, "ASCII")
  protected val io = ByteStreamIO(ioURI, "io")
  protected def query(qry: String) = { codec(io).send(qry); codec(io).recv().get }

  protected def readChannels(qry: String, ch: Ch): Future[ChV[Double]] = {
    if ((ch.min < 1) || (ch.max > nChannels)) throw new IllegalArgumentException("Invalid channel number")
    val r = query(qry) 
    successful( ChV(r.split(",")) { case (i, v) if (ch contains i+1) => (i+1, v.toDouble) } )
  }

  def identity = successful("LakeShore218")

  def readTemp(ch: Ch): Future[ChV[Double]] = readChannels("CRDG?", ch)
}
