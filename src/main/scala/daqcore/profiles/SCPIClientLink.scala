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


package daqcore.profiles

import scala.actors._

import java.net.InetAddress

import daqcore.util._
import daqcore.actors._
import daqcore.servers._
import daqcore.prot.scpi._


trait SCPIClientLink extends Profile with Closeable {
  def queryF(timeout: Long, instr: Instruction*): Future[Option[Response]] =
    srv.!!& (SCPIClientLink.CmdQuery(timeout, instr: _*)) {
      case x: Response => Some(x)
      case Timeout => None
    }

  def queryF(instr: Instruction*): Future[Response] =
    srv.!!& (SCPIClientLink.CmdQuery(Int.MaxValue, instr: _*)) {
      case x: Response => x
    }

  def cmd(instr: Instruction*) : Unit = {
    srv ! SCPIClientLink.CmdOnly(instr: _*)
  }
}


object SCPIClientLink {
  case class CmdQuery(timeout: Long, instr: Instruction*) {
    val request = Request(instr: _*)
    require (request.hasResponse)
  }

  case class CmdOnly(instr: Instruction*) {
    val request = Request(instr: _*)
    require (!request.hasResponse)
  }

  def apply(lnk: MsgIO) = SCPIMsgClient(lnk)

  def apply(host: String, device: String) = SCPIMsgClient(VXI11ClientLink(host, device))

  def apply(lnk: StreamIO) = SCPIMsgClient(lnk)

  def apply(host: String, port: Int) = SCPIMsgClient(GPIBOverStream(InetConnection(host, port)))
}