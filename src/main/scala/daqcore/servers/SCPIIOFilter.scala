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


package daqcore.servers

import akka.actor.Actor.actorOf
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._
import daqcore.prot.scpi.{Request, Response, SCPIParser}


trait SCPIRequestOutputFilter extends OutputFilterServer {
  override def profiles = super.profiles.+[SCPIRequestOutput]
  val outputCompanion = SCPIRequestOutput

  override def target: RawMsgOutput
  val targetCompanion = RawMsgOutput

  def srvSend(request: Request): Unit = {
    target.send(request.getBytes)
  }
}


object SCPIRequestOutputFilter {
  class DefaultSCPIRequestOutputFilter(val target: RawMsgOutput) extends SCPIRequestOutputFilter

  def apply(target: RawMsgOutput, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): SCPIRequestOutput =
    new ServerProxy(sv.linkStart(actorOf(new DefaultSCPIRequestOutputFilter(target)), lc)) with SCPIRequestOutput
}



trait SCPIRequestInputFilter extends InputFilterServer {
  override def profiles = super.profiles.+[SCPIRequestInput]
  val inputCompanion = SCPIRequestInput

  override def source: RawMsgInput
  val sourceCompanion = RawMsgInput

  val parser: SCPIParser = new SCPIParser
  
  def srvProcessInput(data: ByteSeq) = {
    trace("Received %s bytes: %s".format(data.size, loggable(data)))
    
    val request = parser.parseRequest(data)
    trace("Received request: %s".format(loggable(request.toString)))
    Seq(request)
  }
}


object SCPIRequestInputFilter {
  class DefaultSCPIRequestInputFilter(val source: RawMsgInput) extends SCPIRequestInputFilter

  def apply(source: RawMsgInput, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): SCPIRequestInput =
    new ServerProxy(sv.linkStart(actorOf(new DefaultSCPIRequestInputFilter(source)), lc)) with SCPIRequestInput
}



trait SCPIResponseInputFilter extends InputFilterServer {
  override def profiles = super.profiles.+[SCPIResponseInput]
  val inputCompanion = SCPIResponseInput

  override def source: RawMsgInput
  val sourceCompanion = RawMsgInput

  val parser: SCPIParser = new SCPIParser
  
  def srvProcessInput(data: ByteSeq) = {
    trace("Received %s bytes: %s".format(data.size, loggable(data)))
    
    val response = parser.parseResponse(data)
    trace("Received response: %s".format(loggable(response.toString)))
    Seq(response)
  }
}


object SCPIResponseInputFilter {
  class DefaultSCPIResponseInputFilter(val source: RawMsgInput) extends SCPIResponseInputFilter

  def apply(source: RawMsgInput, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): SCPIResponseInput =
    new ServerProxy(sv.linkStart(actorOf(new DefaultSCPIResponseInputFilter(source)), lc)) with SCPIResponseInput
}
