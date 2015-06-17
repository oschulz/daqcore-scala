// Copyright (C) 2010-2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.reflect.{ClassTag, classTag}
import scala.concurrent.{Future, Promise}

import akka.actor.{Actor, ActorRef, ActorRefFactory, Terminated}

import daqcore.actors._
import daqcore.util._


trait EventSource[+A] {
  import EventSource.{EventAction, EventSelector}
  //!!!def recv(): Future[A]

  def subscribe(subscriber: ActorRef): Future[Unit]
  def subscribe(subscriber: ActorRef, select: Seq[Class[_]]): Future[Unit]
  def subscribe(subscriber: ActorRef, selector: EventSelector[A]): Future[Unit]
  def subscribe(action: EventAction[A]): Future[ActorRef]

  def unsubscribe(subscriber: ActorRef): Unit
}


object EventSource {
  type EventAction[A] = PartialFunction[A, Unit]
  type EventSelector[A] = Function[Class[_], Boolean]

  case object selectAll extends Function[Any, Boolean] { def apply(event: Any) = true }


  case class SelectByClass(select: Seq[Class[_]]) extends Function[Any, Boolean] {
    def apply(event: Any) = select.contains(event.getClass) 
  }


  class EventActionExecutor[-A](val action: EventAction[A]) extends Actor with Logging {
    def receive = {
      case event =>
        val evt = event.asInstanceOf[A]
        if (action.isDefinedAt(evt)) {
          log.trace("Received Event " + evt)
          action(evt)        
        }
    }
  }

  object EventActionExecutor {EventSource
    def apply[A](action: EventAction[A])(implicit rf: ActorRefFactory): ActorRef =
      actorOf(new EventActionExecutor(action))

    def apply[A](name: String)(action: EventAction[A])(implicit rf: ActorRefFactory): ActorRef =
      actorOf(new EventActionExecutor(action), name)
  }


  class EventSourceImpl[A] extends EventSource[A] with TypedActorImpl with TypedActorReceive {
    /*

    counter

    recv(newerThan counter)

    cache/history

    */


    val subscribers = collection.mutable.Map[ActorRef, EventSelector[A]]()

    def subscribe(subscriber: ActorRef): Future[Unit] =
      subscribe(subscriber, selectAll)

    def subscribe(subscriber: ActorRef, select: Seq[Class[_]]): Future[Unit] =
      subscribe(subscriber, SelectByClass(select))

    def subscribe(subscriber: ActorRef, selector: EventSelector[A]): Future[Unit] = {
      context.watch(subscriber)
      subscribers += subscriber -> selector
      successful({})
    }

    def subscribe(action: EventAction[A]): Future[ActorRef] = {
      val actionExecutor = EventActionExecutor(action)
      subscribe(actionExecutor, selectAll)
      successful(actionExecutor)
    }

    def unsubscribe(subscriber: ActorRef): Unit = {
      context.unwatch(subscriber)
      subscribers -= subscriber
    }

    protected def emitEvent(event: A): Unit = {
      subscribers foreach { case (subscriber, selector) =>
        if (selector(event.getClass)) subscriber ! event }
    }

    override def receive = extend(super.receive) {
      case terminated: Terminated => subscribers -= terminated.actor
    }
  }
}



trait EventSink[-A] {
  def sendEvent(event: A): Future[Unit] = {
    //!!!!!
    null
  }
}


object EventSink {
}



trait EventIO[A] extends EventSource[A] with EventSink[A]


object EventIO {
  //!!! def apply(name: String): EventForwarder = ....
}



trait EventForwarder[A] extends EventIO[A] {

}


object EventForwarder {
  //!!! def apply(name: String): EventForwarder = ....
}
