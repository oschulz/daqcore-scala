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

import java.net.InetAddress

import akka.dispatch.Future
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}
import akka.config.Supervision.{OneForOneStrategy, AllForOneStrategy}

import daqcore.util._
import daqcore.actors._
import daqcore.servers._
import daqcore.prot.scpi._


trait SCPIClientLink extends Profile with Closeable {
  def queryF(instr: Instruction*)(timeout: Long = defaultTimeout): Future[Response] =
    srv.!!>(SCPIClientLink.CmdQuery(instr: _*), timeout)

  def cmd(instr: Instruction*) : Unit =
    srv ! SCPIClientLink.CmdOnly(instr: _*)
}


object SCPIClientLink {
  case class CmdQuery(instr: Instruction*) extends ActorQuery[Response] {
    val request = Request(instr: _*)
    require (request.hasResponse)
  }

  case class CmdOnly(instr: Instruction*) extends ActorCmd {
    val request = Request(instr: _*)
    require (!request.hasResponse)
  }

  def apply(msgLnk: RawMsgIO, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): SCPIClientLink =
    SCPIMsgClient(msgLnk, sv, lc)
  
  def overInetStream(addr: InetSockAddr, timeout: Long = 10000, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): SCPIClientLink = {
    val msgLnk = GPIBStreamIO.overInetStream(addr, timeout, sv, lc)
    SCPIClientLink(msgLnk, sv, lc)
  }
}
