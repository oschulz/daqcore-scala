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
  def profileOf[T <: Profile : ClassManifest] =
    ProfileInfo.apply[T]


  /** execute code in an actor, then wait for it to exit.
  Useful to execute code that relies on react magic (like Future.respond)
  in a standard thread (e.g. the main thread). */
  def actwait (body: => Unit) {
    import scala.actors._, scala.actors.Actor._
    
    case object Finished
    val trapE = self.trapExit
    self.trapExit = true
    val a = new Actor { def act = body }
    link(a)
    a.start
    receive {
      case Exit(a, 'normal) =>
      case Exit(a, reason) => exit(reason)
    }
    self.trapExit = trapE
  }

  def start[A <: scala.actors.Actor](a: A) : A =
    { a.start(); a }
  
  def startLinked[A <: scala.actors.Actor](a: A) : A =
    { scala.actors.Actor.link(a); a.start(); a }
  
  def kill(a: scala.actors.AbstractActor, reason: AnyRef) : Unit =
    start(new KillActor(a, reason))

  implicit def abstractActorOps(actor: scala.actors.AbstractActor) =
    new AbstractActorOps(actor)

  implicit def actorOps(actor: scala.actors.Actor) =
    new ActorOps(actor)

    
  def unlinkedF[T](body: => T): scala.actors.Future[T] =
    scala.actors.contrib.daqcore.Futures.unlinkedF(body)

  def unlinkedF[T](resp: Responder[T]): scala.actors.Future[T] =
    scala.actors.contrib.daqcore.Futures.unlinkedF(resp)

  def linkedF[T](body: => T): scala.actors.Future[T] =
    scala.actors.contrib.daqcore.Futures.linkedF(body)

  def linkedF[T](resp: Responder[T]): scala.actors.Future[T] =
    scala.actors.contrib.daqcore.Futures.linkedF(resp)
}
