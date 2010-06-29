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


package object actors {

  @volatile implicit var defaultTimeout: TimeoutSpec = NoTimeout

  type MsgTarget = { def !(msg: Any): Unit }

  def profileOf[T <: Profile : ClassManifest] =
    ProfileInfo.apply[T]

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
    a.!?>() {
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
  
  def sendAfter(time: Long, dest: scala.actors.AbstractActor, msg: Any) = {
    import scala.actors._, scala.actors.Actor._
    actor { reactWithin(time) { case TIMEOUT => dest ! msg } }
  }

  implicit def abstractActorOps(actor: scala.actors.AbstractActor) =
    new AbstractActorOps(actor)

  implicit def actorOps(actor: scala.actors.Actor) =
    new ActorOps(actor)

  implicit def ft[T](future: scala.actors.Future[T]) =
    Ft(future)


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

}
