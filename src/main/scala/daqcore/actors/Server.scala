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
  
  private var active = false
  
  /** Servers must implement this as a stable, immutable
  Set of Profiles */
  protected val profiles: Set[ProfileInfo]

  /** Servers may override this */
  protected def started(): Unit =
    { debug("Server %s started".format(self)) }

  /** Servers may override this */
  protected def restarted(): Unit =
    { debug("Server %s restarted".format(self)) }

  /** Servers may override this */
  protected def init(): Unit =
    { trace("Server %s init".format(self)) }

  /** Servers must implement this */
  protected def serve: PartialFunction[Any, Unit]

  /** Servers may override this, deinit is called once for every call of init */
  protected def deinit(): Unit =
    { trace("Server %s deinit".format(self)) }

  /** Servers may override this */
  protected def killed(msg: Exit): Unit =
    { debug("Server %s killed: %s".format(self, msg)) }

  /** Servers may override this */
  protected def shutdown(): Unit =
    { debug("Server %s shut down".format(self)) }
  
  protected def handleGeneric: PartialFunction[Any, Unit] = {
    case GetProfiles => reply(profiles)
    case Exit(_, 'normal) => { deinit(); shutdown(); active = false; exit() }
    case e @ Exit(_, reason) => { deinit(); killed(e); exit(reason) }
  }

  protected def handleUnknown: PartialFunction[Any, Unit] = {
    case _ => throw new RuntimeException("unknown message")
  }
  
  def act() = {
    trapExit = true
    
    if (!active) { active = true; started() } else restarted()
    init()
    
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
  
  lazy val profiles = as[Set[ProfileInfo]] (self !? GetProfiles)

  protected def supports(profile: ProfileInfo) = profiles.contains(profile)
  
  def profile[T <: ServerProxy: ClassManifest] = {
    val p = ProfileInfo.of[T]
    if (!supports(p)) throw new IllegalArgumentException("Proxy target actor does not support profile " + p)
  }

  def as[T](body: => Any) = (body).asInstanceOf[T]
}
