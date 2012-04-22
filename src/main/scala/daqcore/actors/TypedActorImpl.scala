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
import akka.dispatch.{Future, Promise}

import daqcore.util._
import TypedActorTraits._


trait TypedActorBasics
{
  implicit def dispatcher = TypedActor.dispatcher
  implicit def context: ActorContext = TypedActor.context

  def self[A <: AnyRef]: A = TypedActor.self[A]
  def selfRef: ActorRef = context.self
  def actorSystem: ActorSystem = context.system

  def actorRef(obj: AnyRef) = TypedActor(actorSystem).getActorRefFor(obj)

  def actorFor(path: Iterable[String]): ActorRef = actorSystem.actorFor(path)
  def actorFor(path: String): ActorRef = actorSystem.actorFor(path)
  def actorFor(path: ActorPath): ActorRef = actorSystem.actorFor(path)

  def typedActor[T <: AnyRef](aref: ActorRef)(implicit mf: ClassManifest[T]) = TypedActor(actorSystem).typedActorOf(TypedProps[T](), aref)

  def successful[A](x: A): Future[A] = Promise successful x
  
  def selfStop(): Unit = context.stop(selfRef)

  def schedule(initialDelay: Duration, frequency: Duration, receiver: ActorRef, message: Any): Cancellable =
    actorSystem.scheduler.schedule(initialDelay, frequency, receiver, message)
  
  def scheduleOnce(delay: Duration, receiver: ActorRef, message: Any): Cancellable =
    actorSystem.scheduler.scheduleOnce(delay, receiver, message)
}


trait TypedActorImpl extends TypedActorBasics with Logging with Profiling
  with PreStart with PostStop with PreRestart with PostRestart
{
  override implicit val context: ActorContext = super.context

  val selfId = "Typed%s(%s)".format(selfRef, this.getClass)

  log.debug("Creating object for %s".format(selfId))
  
  def init(): Unit = {
    withCleanup
      { log.debug("%s initializing".format(selfId)) }
      { log.debug("%s cleaned up".format(selfId)) }
  }

  def preStart(): Unit = {
    log.debug("preStart of " + selfId)
    init()
  }

  def postRestart(reason: Throwable): Unit = {
    log.debug("postRestart of " + selfId + ", caused by: " + reason)
  }
  
  def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.debug("preRestart of " + selfId + ", caused by: " + reason)

    try {
      runCleanupActions()
    } finally {
    }
  }

  def postStop(): Unit = {
    log.debug("postStop of " + selfId)

    try {
      runCleanupActions()
      runShutdownActions()
    } finally {
    }
  }
  
  
  def atCleanup(body: => Unit): Unit = { cleanupActions = { (() => body) :: cleanupActions } }

  def atShutdown(body: => Unit): Unit = { shutdownActions = { (() => body) :: shutdownActions } }
  
  def withCleanup (initBody: => Unit)(cleanupBody: => Unit) {
    atCleanup { cleanupBody }
    initBody
  }

  type SupportsClose = { def close(): Unit }

  def closeAtCleanup[T <: SupportsClose](res: T): T = {
    atCleanup { () => res.close() }
    res
  }


  protected[actors] var cleanupActions: List[() => Unit] = Nil

  protected[actors] def runCleanupActions(): Unit = {
    while (cleanupActions != Nil) {
      val action::rest = cleanupActions
      cleanupActions = rest
      try { action() } catch { case e => log.error(e.toString) }
    }
  }

  protected[actors] var shutdownActions: List[() => Unit] = Nil

  protected[actors] def runShutdownActions(): Unit = {
    while (shutdownActions != Nil) {
      val action::rest = shutdownActions
      shutdownActions = rest
      try { action() } catch { case e => log.error(e.toString) }
    }
  }

}


trait CloseableTA extends Closeable {
  def isOpen(): Future[Boolean]
}


trait CloseableTAImpl extends Closeable with TypedActorImpl {
  def isOpen(): Future[Boolean] = successful(true)

  def close(): Unit = {
    log.debug("Closing %s".format(selfId))
    selfStop()
  }
}
