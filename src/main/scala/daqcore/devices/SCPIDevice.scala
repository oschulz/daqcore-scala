// Copyright (C) 2011-2013 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import daqcore.util._
import daqcore.io._
import daqcore.actors._

import daqcore.io.prot.scpi._, daqcore.io.prot.scpi.mnemonics._


trait SCPIDevice extends Device {
  def cmd(instr: Instruction*): Unit
  def qry(instr: Instruction*): Future[Response]

  def rawCmd(req: ByteString): Unit
  def rawQry(req: ByteString): Future[ByteString]
}


trait SCPIDeviceImpl extends SCPIDevice with CloseableTAImpl {
  import daqcore.defaults.defaultTimeout //!! get actor default timeout somehow?

  val io: ByteStreamIO
  def msgIO = SCPIStreamFramer(io)

  protected val parser = new SCPIParser

  def respNRfDouble: PartialFunction[Response, Double] = _ match { case Response(Result(NRf(r))) => r }
  def respNR1Int: PartialFunction[Response, Int] = _ match { case Response(Result(NR1(r))) => r }
  def respNR1Boolean: PartialFunction[Response, Boolean] = _ match { case Response(Result(NR1(r))) => if (r>0) true else false }
  def respNR1One: PartialFunction[Response, Unit] = _ match { case Response(Result(NR1(1))) => {} }
  def respNR1Zero: PartialFunction[Response, Unit] = _ match { case Response(Result(NR1(0))) => {} }
  
  def cmd(instr: Instruction*) = rawCmd(Request(instr: _*).getBytes)
  def qry(instr: Instruction*) = rawQry(Request(instr: _*).getBytes) map parser.parseResponse

  def rawCmd(req: ByteString) = { log.trace(req.decodeString("ASCII")); msgIO.send(req) }
  def rawQry(req: ByteString) = { log.trace(req.decodeString("ASCII")); msgIO.send(req); msgIO.recv() } 
}



trait SCPICompliantDevice extends SCPIDevice {
  def reset(): Future[Unit]
}


trait SCPICompliantDeviceImpl extends SCPICompliantDevice with SCPIDeviceImpl with SyncableImpl {
  def identity() = qry(IDN?) map { case Response(Result(AARD(idn))) => idn }

  def reset() = qry(RST!, OPC?) map respNR1One

  override def sync() = cmd(WAI!)

  override def getSync() = qry(OPC?) map respNR1One
}
