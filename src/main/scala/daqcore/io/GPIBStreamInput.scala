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


package daqcore.io

import java.io.{File}

import akka.actor.Actor.actorOf
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}

import daqcore.actors._
import daqcore.util._
import daqcore.io.prot.scpi.{GPIBStreamExtractor}


trait GPIBStreamInput extends InputFilterServer {
  override def profiles = super.profiles.+[RawMsgInput]
  val inputCompanion = RawMsgInput

  override def source: ByteStreamInput
  val sourceCompanion = ByteStreamInput
  
  val extractor = GPIBStreamExtractor()
  
  override def needMoreInput = extractor.unfinished
  
  def srvProcessInput(data: ByteSeq) = {
    trace("doHandleInput(%s)".format(loggable(data)))
    val extracted = extractor(data)
    for (msg <- extracted) {
      trace("Complete message of length %s available: [%s]".format(msg.length, loggable(ByteCharSeq(msg: _*))))
    }
    extracted
  }
}


object GPIBStreamInput {
  class DefaultGPIBStreamInput(val source: ByteStreamInput) extends GPIBStreamInput
  
  def apply(stream: ByteStreamInput, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): RawMsgInput = {
    new ServerProxy(sv.linkStart(actorOf(new DefaultGPIBStreamInput(stream)), lc)) with RawMsgInput
  }
}
