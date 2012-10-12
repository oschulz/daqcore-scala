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

import scala.reflect.{ClassTag, classTag}
import scala.concurrent.{Future, Promise}
import akka.actor._

import daqcore.util._
import TypedActorTraits._


trait TypedActorBasics
{
  implicit def dispatcher = TypedActor.dispatcher
  implicit def context: ActorContext = TypedActor.context
  implicit def selfRef: ActorRef = context.self

  def self[A <: AnyRef]: A = TypedActor.self[A]
  def actorSystem: ActorSystem = context.system

  def successful[A](x: A): Future[A] = Promise successful x future
  
  def selfStop(): Unit = context.stop(selfRef)

  def schedule(initialDelay: Duration, frequency: Duration, receiver: ActorRef, message: Any): Cancellable =
    actorSystem.scheduler.schedule(initialDelay, frequency, receiver, message)
  
  def scheduleOnce(delay: Duration, receiver: ActorRef, message: Any): Cancellable =
    actorSystem.scheduler.scheduleOnce(delay, receiver, message)
}


abstract class TypedActorCompanion[+A <: AnyRef : ClassTag] {
  def apply(aref: ActorRef)(implicit sys: ActorSystem) = typedActor[A](aref)
}


trait TypedActorImpl extends TypedActorBasics with Logging with Profiling
  with PreStart with PostStop with PreRestart with PostRestart with TypedActorReceive
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
    init()
  }
  
  def stopChildrenOnRestart: Boolean = true

  def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.debug("preRestart of " + selfId + ", caused by: " + reason)

    try {
      runCleanupActions()
    } finally {
    }

    if (stopChildrenOnRestart) context.children foreach context.stop
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
