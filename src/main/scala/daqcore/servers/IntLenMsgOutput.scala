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


trait IntLenMsgOutput extends OutputFilterServer {
  override def profiles = super.profiles.+[RawMsgOutput]
  val outputCompanion = RawMsgOutput

  override def target: ByteStreamOutput
  val targetCompanion = ByteStreamOutput

  def srvSend(data: ByteSeq): Unit = if (!data.isEmpty) {
    val bld = ByteSeqBuilder()
    BigEndian.putInt(bld, data.length.toInt)
    bld ++= data
    target.send(bld.result())
  }
}


object IntLenMsgOutput {
  class DefaultIntLenMsgOutput(val target: ByteStreamOutput) extends IntLenMsgOutput
  
  def apply(stream: ByteStreamOutput, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): RawMsgOutput =
    new ServerProxy(sv.linkStart(actorOf(new DefaultIntLenMsgOutput(stream)), lc)) with RawMsgOutput
}
