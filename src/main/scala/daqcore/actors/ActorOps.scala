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

import scala.actors._

import daqcore.util.classMF


class ActorOps(wrapped: AbstractActor) {
  def !!?(msg: Any): Future[Any] =
    this.!!& (msg) { case x => x }

  def !!&[A](msg: Any) (handler: PartialFunction[Any, A]): Future[A] =
    (wrapped !! (msg,handler)).asInstanceOf[Future[A]] // Ugly hack to correct for insufficient return specification of AbstractActor.!!

  def !!^[A: ClassManifest](msg: Any): Future[A] = {
    val mf = classManifest[A]
    this.!!& (msg) { case x if (classMF(x) <:< mf) => x.asInstanceOf[A] }
  }
}
