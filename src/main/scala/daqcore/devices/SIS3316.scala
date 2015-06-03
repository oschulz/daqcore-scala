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


package daqcore.devices

import scala.concurrent.{Future, Promise, ExecutionContext}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import scala.collection.breakOut
import scala.collection.immutable.Queue
import akka.actor._

import daqcore.actors._, daqcore.actors.TypedActorTraits._
import daqcore.util._
import daqcore.io._
import daqcore.io.memory._


trait SIS3316 extends Device {
  def memory: Future[SIS3316Memory]

  def serNo(): Future[String]
  def internalTemperature(): Future[Double]

  def startFIFOReadTransfer(ch: Int, bank: Int, relAddr: Int = 0): Future[Unit]
  def resetFIFO(ch: Int): Future[Unit]
  def readFIFOData(ch: Int, nWords: Int): Future[ByteString]

  //def vmeFWVersion: Future[String]
}

object SIS3316 extends DeviceCompanion[SIS3316] {
  def impl = { case uri => new SIS3316Impl(uri.toString) }

  class SIS3316Impl(vmeURI: String) extends SIS3316
    with CloseableTAImpl with SyncableImpl with LocalECTypedActorImpl
  {
    import SIS3316Impl._
    val registers = SIS3316Memory.registers

    val mem = SIS3316Memory(vmeURI, "memory")

    def memory = successful(mem)


    def identity = localExec( async {
      val modId = mem.readConv(registers.modid.module_id)
      val fwType = mem.readConv(registers.modid.module_id)
      s"SIS${await(modId)}-${await(fwType)}"
    } (_) )


    def serNo = mem.read(registers.serial_number_reg) map { x => x.toString }

    def internalTemperature() = mem.readConv(registers.internal_temperature_reg.temperature)


    def startFIFOReadTransfer(ch: Int, bank: Int, relAddr: Int) = {
      import SIS3316Memory.registers.DataTransferCtlReg.Cmd.{Read => fifoReadCmd}
      import SIS3316Memory.dataRegion.{fpgaChMemSpaceSel, fpgaChFIFOAddrOffset}

      val group = fpgaNum(ch)
      val grpCh = fpgaCh(ch)
      mem.sync()
      mem.write(registers.data_transfer_ctrl_reg(group).tiedValue(
        cmd = fifoReadCmd, mem_space_sel = fpgaChMemSpaceSel(grpCh),
        mem_addr = fpgaChFIFOAddrOffset(grpCh, bank).toInt + relAddr
      ))
    }


    def resetFIFO(ch: Int) = {
      import SIS3316Memory.registers.DataTransferCtlReg.Cmd.{Reset => fifoResetCmd}
      val group = fpgaNum(ch)
      mem.sync()
      mem.write(registers.data_transfer_ctrl_reg(group).tiedValue(cmd = fifoResetCmd))
    }


    def readFIFOData(ch: Int, nBytes: Int) = {
      val group = fpgaNum(ch)
      val readAddr = SIS3316Memory.dataRegion.fifo(group).from
      mem.sync()
      mem.readBulk(readAddr, nBytes)
    }

    ///def vmeFWVersion = ...
  }


  object SIS3316Impl {
    def fpgaNum(devCh: Int) = (devCh - 1) / 4 + 1

    def fpgaCh(devCh: Int) = (devCh - 1) % 4 + 1
  }
}
