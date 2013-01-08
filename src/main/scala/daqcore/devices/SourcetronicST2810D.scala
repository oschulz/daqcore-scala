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


trait SourcetronicST2810D extends SCPIDevice {
  def readPrimary(): Future[Double] 
}

object SourcetronicST2810D extends DeviceCompanion[SourcetronicST2810D] {
  def impl = { case uri => new SourcetronicST2810DImpl(uri.toString) }
}



class SourcetronicST2810DImpl(ioURI: String) extends SourcetronicST2810D
  with SCPIDeviceImpl with SyncableImpl
{
  val io = ByteStreamIO(ioURI, "io")

  def identity = successful("SourcetronicST2810D")

  def readPrimary(): Future[Double] =
    qry(~FETCh?) map { case Response(Result(NRf(primary), NRf(secondary))) => primary }
}
