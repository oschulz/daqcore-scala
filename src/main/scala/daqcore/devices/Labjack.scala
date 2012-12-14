// Copyright (C) 2012-2013 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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
import daqcore.util._
import daqcore.io._


trait Labjack extends Device {
  def readBooleans(address: Int, count: Int): Future[Seq[Boolean]]
  def readShorts(address: Int, count: Int): Future[Seq[Short]]
  def readInts(address: Int, count: Int): Future[Seq[Int]]
  def readFloats(address: Int, count: Int): Future[Seq[Float]]

  def writeBooleans(address: Int, values: Boolean*): Future[Int]
  def writeShorts(address: Int, values: Short*): Future[Int]
  def writeInts(address: Int, values: Int*): Future[Int]
  def writeFloats(address: Int, values: Float*): Future[Int]
  
  def readAIn(ch: Ch = Ch(0 to 3)): Future[ChV[Double]] 
}


object Labjack {
}


trait LabjackUE9 extends Labjack {
}

object LabjackUE9 extends DeviceCompanion[LabjackUE9] {
  def impl = { case uri => new LabjackUE9Impl(uri.toString) }
}



class LabjackUE9Impl(busURI: String) extends LabjackUE9
  with CloseableTAImpl with SyncableImpl
{
  import daqcore.io.prot.modbus.BigEndianShortCoding._

  val bus = ModbusServer(busURI, "modbus")
  val slave = 0 // No modbus slave address required for Modbus TCP

  protected def readFloatChannels(addrOffset: Int, nChannels: Int, ch: Ch): Future[ChV[Double]] = {
    val (from, until) = (ch.min, ch.max + 1)
    if ((from < 0) || (until > nChannels)) throw new IllegalArgumentException("Invalid channel number")
    readFloats(addrOffset + from, until - from) map { r => ChV(r) { case (i, v) if (ch contains i) => (i, v.toDouble) } }
  }
  
  def identity = successful("LabjackUE9")

  def readShorts(address: Int, count: Int) =
    bus.readRegisters(0, address, count)
 
  def readBooleans(address: Int, count: Int) =
    readShorts(address, count) map { case BooleanEnc(v @ _*) => v }

  def readInts(address: Int, count: Int) =
    readShorts(address, count * 2) map { case IntEnc(v @ _*) => v }

  def readFloats(address: Int, count: Int) =
    readShorts(address, count * 2) map { case FloatEnc(v @ _*) => v }

  def writeShorts(address: Int, values: Short*): Future[Int] =
    bus.writeRegisters(0, address, values.toArrayVec)

  def writeBooleans(address: Int, values: Boolean*): Future[Int] =
    writeShorts(address, BooleanEnc(values : _*) : _*)

  def writeInts(address: Int, values: Int*): Future[Int] =
    writeShorts(address, IntEnc(values : _*) : _*)

  def writeFloats(address: Int, values: Float*): Future[Int] =
    writeShorts(address, FloatEnc(values : _*) : _*)

  def readAIn(ch: Ch): Future[ChV[Double]] = readFloatChannels(0, 14, ch)
}
