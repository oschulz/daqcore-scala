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


package daqcore.actors

import daqcore.util._


abstract trait QueueingServer extends Server {
  class ReplyQueue {
    queue => 
    protected val targetQueue = collection.mutable.Queue[MsgTarget]()
    protected val replyQueue = collection.mutable.Queue[(Any, () => Unit)]()
    
    protected def sendReplies(): Unit = {
      while (!targetQueue.isEmpty && !replyQueue.isEmpty) {
        val (target, (msg, action)) = (targetQueue.dequeue(), replyQueue.dequeue())
        trace("Sending msg to reply target %s queued in %s: %s".format(target, queue, loggable(msg)))
        target ! msg
        action
      }
    }
    
    def addTarget(target: MsgTarget): Unit = {
      trace("Adding target %s to reply queue %s".format(target, queue))
      targetQueue.enqueue(target)
      sendReplies()
    }
    
    def addReply(msg: Any)(action: => Unit): Unit = {
      replyQueue.enqueue((msg, () => action))
      sendReplies()
    }
    
    def pendingRequests = targetQueue.size
    def pendingReplies = targetQueue.size
  }
}
