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

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._
import daqcore.prot.scpi.{SCPIParser, StreamMsgTerm}


abstract trait QueueingServer extends Server {
  class ReplyQueue {
    val targetQueue = collection.mutable.Queue[MsgTarget]()
    val replyQueue = collection.mutable.Queue[(Any, () => Unit)]()
    
    protected def sendReplies(): Unit = {
      while (!targetQueue.isEmpty && !replyQueue.isEmpty) {
        val (target, (msg, action)) = (targetQueue.dequeue(), replyQueue.dequeue())
        target ! msg
        action
      }
    }
    
    def addTarget(target: MsgTarget): Unit = {
      targetQueue.enqueue(target)
      sendReplies()
    }
    
    def addReply(msg: Any)(action: => Unit): Unit = {
      replyQueue.enqueue((msg, () => action))
      sendReplies()
    }
  }
}
