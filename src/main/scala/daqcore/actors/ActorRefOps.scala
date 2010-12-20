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

import akka.actor.ActorRef
import akka.dispatch.Future


class ActorRefOps(aref: ActorRef) {
  def !>>[R](msg: Any, timeout: Long = aref.timeout)(implicit sender: Option[ActorRef] = None): R = 
    aref.!!![R](msg, timeout)(sender).apply()

  def !!>>[R](msg: Any, timeout: Long = aref.timeout)(implicit sender: Option[ActorRef] = None): Future[R] =
    aref.!!![R](msg, timeout)(sender)

  def !>[R](msg: ActorQuery[R], timeout: Long = aref.timeout)(implicit sender: Option[ActorRef] = None): R = 
    aref.!!![R](msg, timeout)(sender).apply()

  def !!>[R](msg: ActorQuery[R], timeout: Long = aref.timeout)(implicit sender: Option[ActorRef] = None): Future[R] =
    aref.!!![R](msg, timeout)(sender)
}
