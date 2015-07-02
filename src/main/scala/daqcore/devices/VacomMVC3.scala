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

import daqcore.actors._
import daqcore.io._
import daqcore.util._


trait VacomMVC3 extends Device {
  def qry(req: String): Future[String]

  def readPressure(ch: Ch = Ch(1)): Future[ChV[Double]] 
}

object VacomMVC3 extends DeviceCompanion[VacomMVC3] {
  def impl = { case uri => new VacomMVC3Impl(uri.toString) }
}



class VacomMVC3Impl(ioURI: String) extends VacomMVC3
  with CloseableTAImpl with SyncableImpl
{
  implicit def executor = defaultExecContext

  protected val nChannels = 2

  protected val codec = StringLineCodec(LineCodec.CR, "ASCII")
  protected val io = ByteStreamIO(ioURI, "io")

  def qry(req: String): Future[String] = { codec(io).send(req); codec(io).recv() }

  def identity = successful("VacomMVC3")

  def readPressure(channels: Ch): Future[ChV[Double]] = {
    if ((channels.min < 1) || (channels.max > nChannels)) throw new IllegalArgumentException("Invalid channel number")
	val fts = channels.toSeq map { i => qry("RPV%s".format(i)) }
    Future.sequence(fts) map { xs => ChV(xs) { case (i,v) => (i+1, v.split(",")(1).toDouble) } }
  }
}
