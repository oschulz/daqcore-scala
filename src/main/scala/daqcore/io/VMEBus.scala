// Copyright (C) 2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.concurrent.{Future, Promise}

import daqcore.actors._
import daqcore.util._


trait VMEBus extends Syncable {
  import VMEBus._

  def baseAddress: Future[VMEAddress]

  def bulkEncoding(mode: Mode, deviceByteOrder: ByteOrder = BigEndian): Future[ValEncoding]

  def readIntReg(address: VMEAddress, mode: Mode = A32_D32_SCT, deviceByteOrder: ByteOrder = BigEndian): Future[Int]

  def writeIntReg(address: VMEAddress, value: Int, mode: Mode = A32_D32_SCT, deviceByteOrder: ByteOrder = BigEndian): Future[Unit]

  def readIntRegs(addrs: Seq[VMEAddress], mode: Mode = A32_D32_SCT, deviceByteOrder: ByteOrder = BigEndian): Future[ArrayVec[Int]]

  def writeIntRegs(addrValues: Seq[(VMEAddress, Int)], mode: Mode = A32_D32_SCT, deviceByteOrder: ByteOrder = BigEndian): Future[Unit]

  def readBulk(address: VMEAddress, nBytes: Int, mode: Mode): Future[ByteString]

  def writeBulk(address: VMEAddress, data: ByteString, mode: Mode): Future[Unit]
}



object VMEBus extends IOResourceCompanion[VMEBus] {
  type VMEAddress = Long

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
      val SCT         = Value(0, "SCT")
      val BLT         = Value(1, "BLT")
      val MBLT        = Value(2, "MBLT")
      val eeVME       = Value(3, "2eVME")
      val eeSST160    = Value(4, "2eSST160")
      val eeSST267    = Value(5, "2eSST267")
      val eeSST320    = Value(6, "2eSST320")
  }
  
  case class Mode(space: AddressSpace.Value, width: DataWidth.Value, cycle: VMECycle.Value)

  val A32_D32_SCT = Mode(AddressSpace.A32, DataWidth.D32, VMECycle.SCT);
  val A32_D32_BLT = Mode(AddressSpace.A32, DataWidth.D32, VMECycle.SCT);
  val A32_D64_MBLT = Mode(AddressSpace.A32, DataWidth.D64, VMECycle.MBLT);
  val A32_D64_2eVME = Mode(AddressSpace.A32, DataWidth.D64, VMECycle.eeVME);


  def newInstance =
    VMEBusDummy.newInstance orElse
    SISVMEGateway.newInstance

}
