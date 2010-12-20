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

import java.io.{File}

import akka.actor.Actor.actorOf
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._
import daqcore.prot.scpi.{StreamMsgTerm}


trait GPIBStreamOutput extends OutputFilterServer {
  override def profiles = super.profiles.+[RawMsgOutput]
  val outputCompanion = RawMsgOutput

  override def target: ByteStreamOutput
  val targetCompanion = ByteStreamOutput

  protected def srvSend(data: Seq[Byte]): Unit = if (!data.isEmpty) {
    target.send(data)
    if (! (data(data.length-1) == 0x0A)) target.send(StreamMsgTerm)
    target.flush()
  }
}


object GPIBStreamOutput {
  class DefaultGPIBStreamOutput(val target: ByteStreamOutput) extends GPIBStreamOutput

  def apply(stream: ByteStreamOutput, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): RawMsgOutput = {
    new ServerProxy(sv.linkStart(actorOf(new DefaultGPIBStreamOutput(stream)), lc)) with RawMsgOutput
  }
}
