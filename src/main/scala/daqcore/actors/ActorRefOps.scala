// Copyright (C) 2010-2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.reflect.{ClassTag, classTag}
import scala.concurrent.{Future, Promise}
import scala.util.{Try, Success, Failure}

import java.util.concurrent.TimeoutException

import akka.actor.{Actor, ActorRef, ActorContext, ActorRefFactory, ActorSystem}

import daqcore.util.Timeout


class ActorRefOps(val aref: ActorRef) extends AnyVal {
  import ActorRefOps.AskSenderActor

  def stop()(implicit sys: ActorSystem) = sys.stop(aref)

  def typed[T <: AnyRef](implicit ct: ClassTag[T], sys: ActorSystem) = typedActor(aref)

  def askSender(msg: Any)(implicit rf: ActorRefFactory, timeout: Timeout): Future[(_, ActorRef)] = {
    implicit val (scheduler, execContext) = rf match  {
      case s: ActorSystem => (s.scheduler, s.dispatcher)
      case c: ActorContext => (c.system.scheduler, c.dispatcher)
      case _ => throw new IllegalArgumentException(s"Unsupported type of ActorRefFactory for askSender: [$rf.getClass]")
    }

    val result = Promise[(_, ActorRef)]()

    val asker = actorOf(new AskSenderActor(result))

    val timer = scheduler.scheduleOnce(timeout.duration) {
      result tryComplete Failure(new TimeoutException(s"askSender timed out on [$aref] after [${timeout.duration.toMillis} ms]"))
    }

    result.future.onComplete{ _ => try rf.stop(asker) finally timer.cancel() }

    aref.tell(msg, asker)
    result.future
  }
}


object ActorRefOps {
  protected class AskSenderActor(result: Promise[(_, ActorRef)]) extends Actor {
    def receive = {
      case reply => result tryComplete Success( (reply, sender()) )
    }
  }
}
