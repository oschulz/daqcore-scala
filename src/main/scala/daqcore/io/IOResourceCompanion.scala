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


package daqcore.io

import akka.actor.{IO => AkkaIO, _}

import daqcore.actors._


abstract class IOResourceCompanion[+A <: AnyRef : ClassManifest] {
  import IOResourceCompanion._

  def newInstance: PartialFunction[URI, () => A]

  def apply(uri: URI, name: String = "")(implicit rf: ActorRefFactory): A = uri match {
    case AkkaActorPath(path) if akkaURISchemes contains uri.getScheme => typedActor[A](actorFor(path))
    case uri =>
      try { typedActorOf[A](newInstance(uri)(), name) }
      catch { case e: MatchError => throw new IllegalArgumentException("URI \"%s\" not supported".format(uri)) }
  }

  def apply(uri: String, name: String)(implicit rf: ActorRefFactory): A =
    apply(URI(uri), name)

  def apply(uri: String)(implicit rf: ActorRefFactory): A =
    apply(URI(uri))
}


object IOResourceCompanion {
  val akkaURISchemes = Seq("akka", "cluster")
}
