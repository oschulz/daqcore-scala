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


package daqcore

import akka.actor.{ActorRef, Supervisor}
import akka.dispatch.Future
import akka.config.Supervision._

import daqcore.util._


package object actors {
  type MsgTarget = akka.actor.Channel[Any]

  implicit def supervising(wrapped: ActorRef) = Supervising(wrapped)
  implicit def supervising(wrapped: Supervisor) = Supervising(wrapped)

  implicit def ActorRefOps(ref: ActorRef) =
    new ActorRefOps(ref)

  implicit def futureOps[T](future: Future[T]) = new FutureOps(future)

  def sendAfter(time: Long, dest: ActorRef, msg: Any) = {
    import akka.actor.Scheduler 
    import java.util.concurrent.TimeUnit._
    
    Scheduler.scheduleOnce(dest, msg.asInstanceOf[AnyRef], time, MILLISECONDS)
  }

  val defaultSupervisor = {
    val sv = Supervisor(SupervisorConfig(OneForOneStrategy(List(classOf[Throwable]), 3, 1000), Nil))
    object ShutdownHook extends Thread with Logging {
      override def run() = {
        log.info("Shutting down default supervisor")
        sv.shutdown()
      }
    }
    Runtime.getRuntime.addShutdownHook(ShutdownHook)
    sv
  }

  val alwaysRestartSupervisor = {
    val sv = Supervisor(SupervisorConfig(OneForOneStrategy(List(classOf[Throwable]), None, None), Nil))
    object ShutdownHook extends Thread with Logging {
      override def run() = {
        log.info("Shutting down always-restart supervisor")
        sv.shutdown()
      }
    }
    Runtime.getRuntime.addShutdownHook(ShutdownHook)
    sv
  }

  /*

  /** execute code in an actor, then wait for it to exit.
  Useful to execute code that relies on react magic (like Future.respond)
  in a standard thread (e.g. the main thread). */
  def actwait (body: => Unit) {
    val a = new scala.actors.Actor {
      def act = react {
        case e if (! e.isInstanceOf[scala.util.control.ControlThrowable]) =>
          try { body; reply() }
          catch { case e => reply(e) }
      }
    }
    a.start
    ActorRef(a).!?>() {
      case e: Throwable => throw e
      case _ =>
    }
  }

  def start[A <: scala.actors.Actor](a: A) : A =
    { a.start(); a }
  
  def startLinked[A <: scala.actors.Actor](a: A) : A =
    { scala.actors.Actor.link(a); a.start(); a }

  def kill(a: scala.actors.AbstractActor, reason: AnyRef) : Unit =
    start(new KillActor(a, reason))

  def server(initBody: => Unit)(srvFkt: PartialFunction[Any, Unit]) = {
    start( new Server {
      override protected def init() = { super.init; initBody }
      protected def serve() = srvFkt
    } )
  }

  def linkedServer(initBody: => Unit)(srvFkt: PartialFunction[Any, Unit]) = {
    startLinked( new Server {
      override protected def init() = { super.init; initBody }
      protected def serve() = srvFkt
    } )
  }
  
  def spawn(body: => Unit): Unit = scala.actors.Actor.actor(body)
  */
}
