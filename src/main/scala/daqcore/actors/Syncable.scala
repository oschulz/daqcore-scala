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

import akka.actor._, akka.dispatch.Future


trait Syncable extends ServerProfile {
  import Syncable._

  def pause(): Unit = srv ! Pause()

  def sync(timeout: Long = defaultTimeout): Unit = syncF(timeout)get

  def syncF(timeout: Long = defaultTimeout): Future[Unit] =
    srv.!!>(Sync(), timeout)
}


object Syncable {
  case class Pause() extends ActorCmd
  case class Sync() extends ActorQuery[Unit]
}
