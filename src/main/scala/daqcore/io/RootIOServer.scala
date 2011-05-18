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


package daqcore.io

import akka.actor._, akka.actor.Actor._, akka.dispatch.Future
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle, Temporary, OneForOneStrategy, AllForOneStrategy}

import daqcore.util._
import daqcore.actors._
import daqcore.io.prot.rootsys._


class RootIOServer() extends RootSystemServer() {
  override def profiles = super.profiles.+[RootIO]
 
  var nextId = 0

  override def serve = super.serve orElse {
    case RootIO.GetNextId => { nextId += 1; reply(nextId) }
  }
}


object RootIOServer extends Logging {
  def apply(sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): RootIO = {
    val rio = new ServerProxy(sv.linkStart(actorOf(new RootIOServer()), lc)) with RootIO
    val idn = rio.getIdn(60000)
    log.info("Started ROOT-System session: " + idn)
    rio
  }
}
