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
import daqcore.util._
import daqcore.profiles._


trait MsgServer extends Server with MsgSource {
  protected var msgReceiver: Option[(MsgTarget, Boolean)] = None
  protected val msgQueue = collection.mutable.Queue[Any]()
  
  protected def sendQueuedMessages(): Unit = {
    while (msgReceiver.isDefined && ! msgQueue.isEmpty) {
      val Some((receiver, repeat)) = msgReceiver
      val msg = msgQueue.dequeue()
      trace("Sending message %s to %s, repeat: %s".format(loggable(msg), receiver, repeat))
      receiver ! msg
      if (!repeat) msgReceiver = None
    }
  }
  
  protected def enqueueMsg(msg: Any): Unit = msgQueue.enqueue(msg)

  protected def doSetReceiver(receiver: MsgTarget, repeat: Boolean): Unit = {
    msgReceiver = Some((receiver, repeat))
    sendQueuedMessages()
  }

  protected def doSendMsg(msg: Any): Unit = {
    enqueueMsg(msg)
    sendQueuedMessages()
  }
  
  override protected def init() = {
    super.init()
    trapExit = true
  }

  protected def serve = {
    case op @ MsgSource.SetReceiver(receiver, repeat) => {
      trace(op)
      doSetReceiver(receiver, repeat)
    }
    case op @ MsgSource.GetMsg() => {
      trace(op)
      doSetReceiver(replyTarget, false)
    }
  }
}



class MsgSenderServer extends MsgServer with MsgSender {
  override protected def serve = super.serve orElse {
    case MsgSender.SendMsg(msg) =>
      doSendMsg(msg)
  }
}


object MsgSenderServer {
  def apply(): MsgSenderServer =
    start(new MsgSenderServer())
}
