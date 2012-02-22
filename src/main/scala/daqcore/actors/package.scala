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

import akka.actor.{ActorSystem, ActorContext, ActorRef, ActorPath, Actor, ActorRefFactory, Props}
import akka.actor.contrib.daqcore.{TypedActor, TypedProps}
import akka.dispatch.Future
import akka.util.Timeout
import akka.util.duration._


package object actors {
  implicit val daqcoreSystem = ActorSystem("daqcore")

  object TypedActorTraits {
    type Supervisor = TypedActor.Supervisor
    type PreStart = TypedActor.PreStart
    type PostStop = TypedActor.PostStop
    type PreRestart = TypedActor.PreRestart
    type PostRestart = TypedActor.PostRestart
    type Receiver = TypedActor.Receiver
    type DeathWatch = TypedActor.Receiver
  }

  implicit val defaultTimeout = Timeout(5 seconds)

  implicit def actorRefOps(ref: ActorRef) = new ActorRefOps(ref)

  implicit def futureOps[T](future: Future[T]) = new FutureOps(future)

  def actorRef(obj: AnyRef)(implicit asys: ActorSystem) = TypedActor(asys).getActorRefFor(obj)

  def actorFor(path: Iterable[String])(implicit asys: ActorSystem): ActorRef = asys.actorFor(path)
  def actorFor(path: String)(implicit asys: ActorSystem): ActorRef = asys.actorFor(path)
  def actorFor(path: ActorPath)(implicit asys: ActorSystem): ActorRef = asys.actorFor(path)

  def actorOf(creator: => Actor, name: String = "")(implicit rf: ActorRefFactory): ActorRef = {
    if (name.isEmpty) rf.actorOf(Props(creator))
    else rf.actorOf(Props(creator), name)
  }
  
  def typedActor[T <: AnyRef](aref: ActorRef)(implicit mf: ClassManifest[T], sys: ActorSystem) =
    TypedActor(sys).typedActorOf(TypedProps[T](), aref)

  def typedActorOf[R <: AnyRef](creator: => R, name: String = "")(implicit mf: ClassManifest[R], rf: ActorRefFactory): R = {
    val cl = mf.erasure.asInstanceOf[Class[_ >: AnyRef]]
    val props = TypedProps[R](cl, creator)
    val factory = rf match {
      case sys: ActorSystem => TypedActor(sys)
      case con: ActorContext => TypedActor(con)
      case _ => throw new IllegalArgumentException("Expected argument of type ActorSystem or ActorContext.")
    }
    if (name.isEmpty) factory.typedActorOf(props)
    else factory.typedActorOf(props, name)
  }
}
