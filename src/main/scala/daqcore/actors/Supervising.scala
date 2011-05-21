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


package daqcore.actors

import akka.actor.{Actor, Supervisor, ActorRef}
import akka.actor.Actor.actorOf
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}


trait Supervising {
  def link(actorRef: ActorRef): Unit
  
  def linkStart(actorRef: ActorRef): ActorRef = {
    link(actorRef)
    actorRef.start
  }

  def linkStart(actorRef: ActorRef, lifeCycle: LifeCycle): ActorRef = {
    actorRef.lifeCycle = lifeCycle
    linkStart(actorRef)
  }

  def linkStart(actor: => Actor): ActorRef = linkStart(actorOf(actor))

  def linkStart(actor: => Actor, lifeCycle: LifeCycle): ActorRef =
    linkStart(actorOf(actor), lifeCycle)
}


object Supervising {
  def apply(wrapped: ActorRef) = new Supervising {
    def link(target: ActorRef) = wrapped.link(target)
  }

  def apply(wrapped: Supervisor) = new Supervising {
    def link(target: ActorRef) = wrapped.link(target)
  }
}
