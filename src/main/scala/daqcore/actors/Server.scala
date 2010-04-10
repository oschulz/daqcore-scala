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


import scala.actors._, scala.actors.Actor._

import daqcore.util._


trait Server extends DaemonActor with Logging {
  import Server._
  
  /** Servers must implement this as a stable, immutable
  Set of Profiles */
  protected val profiles: Set[Profile]

  /** Servers must implement this */
  protected def serve: PartialFunction[Any, Unit]

  protected def handleGeneric: PartialFunction[Any, Unit] = {
    case GetProfiles => reply(profiles)
  }

  protected def handleUnknown: PartialFunction[Any, Unit] = {
    case _ => exit()
  }
  
  def act() = {
    loop {
      react(
        handleGeneric orElse
        serve orElse
        handleUnknown
      )
    }
  }
}


object Server {
  case object GetProfiles
}



trait ServerProxy extends Proxy with Logging {
  import Server._

  override def self: Actor
  
  lazy val profiles = as[Set[Profile]] (self !? GetProfiles)

  protected def supports(profile: Profile) = profiles.contains(profile)
  
  def profile[T <: ServerProxy: ClassManifest] = {
    val p = Profile.of[T]
    if (!supports(p)) throw new IllegalArgumentException("Proxy target actor does not support profile " + p)
  }

  def as[T](body: => Any) = (body).asInstanceOf[T]
}
