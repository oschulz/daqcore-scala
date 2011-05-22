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


package daqcore.io

import akka.dispatch.Future

import daqcore.util._
import daqcore.actors._


case class VMEInterrupt(vector: Int, missed: Int = 0)


trait VMEBus extends ServerProfile {
  def read(address: Long, count: Long, mode: VMEBus.Mode, timeout: Long = defaultTimeout): ByteSeq =
    readF(address, count, mode, timeout)get

  def readF(address: Long, count: Long, mode: VMEBus.Mode, timeout: Long = defaultTimeout): Future[ByteSeq] =
    srv.!!>(VMEBus.Read(address, count, mode), timeout)

  def write(address: Long, data: ByteSeq, mode: VMEBus.Mode) : Unit =
    srv ! VMEBus.Write(address: Long, data, mode)
  
  def pause(): Unit = srv ! VMEBus.Pause()

  def sync(timeout: Long = defaultTimeout): Unit = syncF(timeout).get

  def syncF(timeout: Long = defaultTimeout): Future[Unit] =
    srv.!!>(VMEBus.Sync(), timeout)

}


object VMEBus {
  object AddressSpace extends Enumeration {
      val A16 = Value(16, "A16")
      val A24 = Value(24, "A24")
      val A32 = Value(32, "A32")
      val A64 = Value(64, "A64")
  }

  object DataWidth extends Enumeration {
      val  D8 = Value( 8,  "D8")
      val D16 = Value(16, "D16")
      val D32 = Value(32, "D32")
      val D64 = Value(64, "D64")
  }
  
  object VMECycle extends Enumeration {
      val SCT     = Value(0,  "SCT")
      val BLT     = Value(1,  "BLT")
      val MBLT    = Value(2, "MBLT")
      val TeVME = Value(3, "2eVME")
  }
  
  case class Mode(space: AddressSpace.Value, width: DataWidth.Value, cycle: VMECycle.Value)

  case class Read(address: Long, count: Long, mode: Mode) extends ActorQuery[ByteSeq]

  case class Write(address: Long, data: ByteSeq, mode: Mode) extends ActorCmd

  case class Pause() extends ActorCmd

  case class Sync() extends ActorQuery[Unit]
}
