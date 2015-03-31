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

import akka.actor._

import daqcore.util._
import daqcore.io.memory._
import daqcore.actors._, daqcore.actors.TypedActorTraits._


trait VMEBusDummy extends VMEBus {}



object VMEBusDummy extends IOResourceCompanion[VMEBusDummy] {
  // Requires VME firmware version V3316-2008 or higher.

  import VMEBus._


  def newInstance = {
    case HostURL("vme-dummy", _, _) => () => new DummyImpl()
  }


  class DummyImpl() extends VMEBusDummy with CloseableTAImpl with SyncableImpl {
    import VMEBus._

    protected val memArray = Array.fill[Byte](0x400000)(0.toByte)

    protected def gwByteOrder = LittleEndian

    def baseAddress: Future[VMEAddress] = successful(0L)

    def bulkEncoding(mode: Mode, deviceByteOrder: ByteOrder = BigEndian) = successful(deviceByteOrder)

    def readIntReg(address: VMEAddress, mode: Mode = A32_D32_SCT, deviceByteOrder: ByteOrder) = {
      log.trace(s"readIntReg($address, $mode, $deviceByteOrder")
      successful(0)
    }


    def writeIntReg(address: VMEAddress, value: Int, mode: Mode = A32_D32_SCT, deviceByteOrder: ByteOrder) = {
      log.trace(s"writeIntReg($address, $value, $mode, $deviceByteOrder")
      successful({})
    }


    def readIntRegs(addrs: Seq[VMEAddress], mode: Mode, deviceByteOrder: ByteOrder) = {
      log.trace(s"readIntRegs(${loggable(addrs)}, $mode, $deviceByteOrder")
      val bld = ArrayVec.newBuilder[Int]
      addrs foreach { address =>
        val bs = ByteString.fromArray(memArray, address.toInt, 4)
        bld += bs.iterator.getInt(deviceByteOrder.nioByteOrder)
      }
      successful(bld.result)
    }


    def writeIntRegs(addrValues: Seq[(VMEAddress, Int)], mode: Mode, deviceByteOrder: ByteOrder) = {
      log.trace(s"writeIntRegs(${loggable(addrValues)}, $mode, $deviceByteOrder")
      addrValues foreach { case (address, value) =>
        val bs = ByteString.newBuilder.putInt(value)(deviceByteOrder.nioByteOrder).result
        bs.iterator.getBytes(memArray, address.toInt, bs.size)
      }
      successful({})
    }


    def readBulk(address: VMEAddress, nBytes: Int, mode: Mode) = {
      log.trace(s"readBulk($address, $nBytes, $mode")
      successful(ByteString.fromArray(memArray, address.toInt, nBytes))
    }


    def writeBulk(address: VMEAddress, data: ByteString, mode: Mode) = {
      log.trace(s"writeBulk($address, ${loggable(data)},$mode")
      data.iterator.getBytes(memArray, address.toInt, data.size)
      successful({})
    }
  }
}
