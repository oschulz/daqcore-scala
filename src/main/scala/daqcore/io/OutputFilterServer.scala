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

import akka.actor._, akka.actor.Actor._, akka.dispatch.Future

import daqcore.actors._
import daqcore.util._


trait OutputFilterServer extends CascadableServer {
  override def profiles = super.profiles.+[GenericOutput]

  val outputCompanion: GenericOutputCompanion
  import outputCompanion._

  def target: GenericOutput
  val targetCompanion: GenericOutputCompanion

  override def init() = {
    super.init()
    clientLinkTo(target.srv)
    atShutdown { target.close() }
  }
  
  override def onServerExit(server: ActorRef, reason: Option[Throwable]) = {
    if ((server == target.srv) && (reason == None)) self.stop()
    else super.onServerExit(server, reason)
  }

  protected def srvSend(data: OutputData): Unit
  
  protected def srvFlush(): Unit = target.flush()

  override def serve = super.serve orElse {
    case Send(data) => srvSend(data)
    case Flush() => srvFlush()
  }
}
