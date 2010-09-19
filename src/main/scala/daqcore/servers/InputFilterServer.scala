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

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._


trait InputFilterServer extends CloseableServer with QueueingServer with GenericInput {
  import inputCompanion._

  def source: GenericInput
  val sourceCompanion: GenericInputCompanion

  val recvQueue = new ReplyQueue

  override protected def init() = {
    super.init()
    addResource(source)
    source.triggerRecv()
  }
  
  protected def srvHandleInput(data: sourceCompanion.InputData)
  
  override def serve = super.serve orElse {
    case RawMsgInput.Recv() => recvQueue addTarget replyTarget

    case sourceCompanion.Received(data) => {
      source.triggerRecv()
      srvHandleInput(data)
    }

    case sourceCompanion.Closed => {
      recvQueue.addReply(Closed){srvClose()}
    }
  }
}
