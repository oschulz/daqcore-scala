// Copyright (C) 2011 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import akka.actor.{Actor, ActorRef}
import akka.actor.Actor.{actorOf, remote, registry}
import akka.serialization.RemoteActorSerialization
import akka.remote.protocol.RemoteProtocol.RemoteActorRefProtocol


case class NTActorRef(protected val bytes: Array[Byte]) {
  import NTActorRef._

  def toActorRef = {
    val rap = RemoteActorRefProtocol.newBuilder.mergeFrom(bytes).build

    val csname = rap.getClassOrServiceName
    if (csname startsWith remote.UUID_PREFIX) {
      val uuid = new akka.actor.Uuid(csname.drop(remote.UUID_PREFIX.length))
      registry.actorFor(uuid) match {
        case Some(aref) => aref
        case None => RemoteActorSerialization.fromBinaryToRemoteActorRef(bytes)
      }
    }
    else RemoteActorSerialization.fromBinaryToRemoteActorRef(bytes)
  }
  
  def this(ref: ActorRef) =
    this(RemoteActorSerialization.toRemoteActorRefProtocol(ref).toByteArray)
}

object NTActorRef {
  val uuidServiceName = """uuid:(.*)""".r

  def apply(ref: ActorRef): NTActorRef = new NTActorRef(ref)
}
