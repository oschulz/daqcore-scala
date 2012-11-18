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


package daqcore

import scala.reflect.{ClassTag, classTag}
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.actor._

import daqcore.util.Timeout


package object actors {
  object TypedActorTraits {
    type Supervisor = TypedActor.Supervisor
    type PreStart = TypedActor.PreStart
    type PostStop = TypedActor.PostStop
    type PreRestart = TypedActor.PreRestart
    type PostRestart = TypedActor.PostRestart
    type Receiver = TypedActor.Receiver
  }

  implicit def actorRefOps(ref: ActorRef) = new ActorRefOps(ref)

  implicit def futureOps[T](future: Future[T]) = new FutureOps(future)
  
  def currentActorSystem(implicit rf: ActorRefFactory) = rf match {
      case system: ActorSystem => system
      case context: ActorContext => context.system
      case _ => throw new IllegalArgumentException("Expected argument of type ActorSystem or ActorContext.")
  }

  def actorRef(obj: AnyRef)(implicit rf: ActorRefFactory) = TypedActor(currentActorSystem(rf)).getActorRefFor(obj)

  def actorFor(path: Iterable[String])(implicit rf: ActorRefFactory): ActorRef = currentActorSystem(rf).actorFor(path)
  def actorFor(path: String)(implicit rf: ActorRefFactory): ActorRef = currentActorSystem(rf).actorFor(path)
  def actorFor(path: ActorPath)(implicit rf: ActorRefFactory): ActorRef = currentActorSystem(rf).actorFor(path)

  def actorOf(creator: => Actor, name: String = "")(implicit rf: ActorRefFactory): ActorRef = {
    if (name.isEmpty) rf.actorOf(Props(creator))
    else rf.actorOf(Props(creator), name)
  }
  
  def typedActor[T <: AnyRef](aref: ActorRef)(implicit mf: ClassTag[T], rf: ActorRefFactory) =
    TypedActor(currentActorSystem(rf)).typedActorOf(TypedProps[T](), aref)

  def typedActorOf[R <: AnyRef](creator: => R, name: String = "")(implicit mf: ClassTag[R], rf: ActorRefFactory): R = {
    val cl = mf.runtimeClass.asInstanceOf[Class[_ >: AnyRef]]
    val props = TypedProps[R](cl, creator)
    val factory = rf match {
      case sys: ActorSystem => TypedActor(sys)
      case con: ActorContext => TypedActor(con)
      case _ => throw new IllegalArgumentException("Expected argument of type ActorSystem or ActorContext.")
    }
    if (name.isEmpty) factory.typedActorOf(props)
    else factory.typedActorOf(props, name)
  }
  
  def spawn(body: ActorContext => Unit)(implicit rf: ActorRefFactory): ActorRef = ForkedTask(body)(rf)
  def spawn(body: => Unit)(implicit rf: ActorRefFactory): ActorRef = ForkedTask(body)(rf)


  def schedule(initialDelay: FiniteDuration, frequency: FiniteDuration)(f: => Unit)(implicit asys: ActorSystem): Cancellable =
    asys.scheduler.schedule(initialDelay, frequency)(f)(asys.dispatcher)

  def schedule(initialDelay: FiniteDuration, frequency: FiniteDuration, receiver: ActorRef, message: Any)(implicit asys: ActorSystem): Cancellable =
    asys.scheduler.schedule(initialDelay, frequency, receiver, message)(asys.dispatcher)
  
  def scheduleOnce(delay: FiniteDuration)(f: => Unit)(implicit asys: ActorSystem): Cancellable =
    asys.scheduler.scheduleOnce(delay)(f)(asys.dispatcher)

  def scheduleOnce(delay: FiniteDuration, receiver: ActorRef, message: Any)(implicit asys: ActorSystem): Cancellable =
    asys.scheduler.scheduleOnce(delay, receiver, message)(asys.dispatcher)
}
