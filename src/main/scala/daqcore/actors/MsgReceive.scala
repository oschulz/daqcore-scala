// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import akka.actor._
import akka.util.Duration
import akka.util.duration._

import daqcore.util.Logging


trait MsgReceive extends TypedActor.Receiver with Logging {
  import akka.actor.contrib.daqcore.UnhandledMsgBehaviour
  import MsgReceive.ExecScheduled
  
  type Receiver = PartialFunction[(Any, ActorRef), Unit]

  def msgReceive: Receiver =
    { case (MsgReceive.NoMatch, _) => }

  def scheduleSelf(initialDelay: Duration, frequency: Duration)(f: => Unit): Cancellable =
    TypedActor.context.system.scheduler.schedule(initialDelay, frequency, TypedActor.context.self, ExecScheduled(() => f))

  def scheduleSelf(initialDelay: Long, frequency: Long)(f: => Unit): Cancellable =
    scheduleSelf(initialDelay milliseconds, frequency milliseconds)(f)

  def scheduleSelfOnce(delay: Duration)(f: => Unit): Cancellable =
    TypedActor.context.system.scheduler.scheduleOnce(delay, TypedActor.context.self, ExecScheduled(() => f))

  def scheduleSelfOnce(delay: Long)(f: => Unit): Cancellable =
    scheduleSelfOnce(delay milliseconds)(f)

  def onReceive(message: Any, sender: ActorRef): Unit = (message, sender) match {
    case (ExecScheduled(body), _) => {
      log.trace("Executing scheduled action")
      body()
    }
    case msg_sender => {
      if (msgReceive isDefinedAt msg_sender) msgReceive(msg_sender)
      else UnhandledMsgBehaviour.unhandled(message, sender, TypedActor.context)
    }
  }
  
  def extend(receiver: Receiver)(extension: Receiver) =
    extension orElse receiver
}


object MsgReceive {
  private object NoMatch

  private [actors] case class ExecScheduled(body: () => Unit)
}
