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

import scala.language.postfixOps

import scala.concurrent.duration._
import akka.actor._

import daqcore.util._


trait TypedActorReceive extends TypedActor.Receiver with Logging {
  import akka.actor.contrib.daqcore.UnhandledMsgBehaviour
  import TypedActorReceive.{Receiver, ExecScheduled}
  
  def receive: Receiver = { case _ if false => }

  def scheduleSelf(initialDelay: FiniteDuration, frequency: FiniteDuration)(f: => Unit): Cancellable =
    TypedActor.context.system.scheduler.schedule(initialDelay, frequency, TypedActor.context.self, ExecScheduled(() => f))(TypedActor.dispatcher)

  def scheduleSelf(initialDelay: Long, frequency: Long)(f: => Unit): Cancellable =
    scheduleSelf(initialDelay milliseconds, frequency milliseconds)(f)

  def scheduleSelfOnce(delay: FiniteDuration)(f: => Unit): Cancellable =
    TypedActor.context.system.scheduler.scheduleOnce(delay, TypedActor.context.self, ExecScheduled(() => f))(TypedActor.dispatcher)

  def scheduleSelfOnce(delay: Long)(f: => Unit): Cancellable =
    scheduleSelfOnce(delay milliseconds)(f)

  final def onReceive(message: Any, sender: ActorRef): Unit = {
    assert ( sender == TypedActor.context.sender )

    message match {
      case message => {
        if (receive isDefinedAt message) receive(message)
        else message match {
          case ExecScheduled(body) => {
            log.trace("Executing scheduled action")
            body()
          }
          case DoCrash(reason) => throw reason
          case _ => UnhandledMsgBehaviour.unhandled(message, sender, TypedActor.context)
        }
      }
    }
  }
  
  def extend(receiver: Receiver)(extension: Receiver) =
    extension orElse receiver
}


object TypedActorReceive {
  private [actors] case class ExecScheduled(body: () => Unit)

  type Receiver = PartialFunction[Any, Unit]
}


case class DoCrash(reason: Throwable = new RuntimeException("Received DoCrash, crashing now as requested"))
