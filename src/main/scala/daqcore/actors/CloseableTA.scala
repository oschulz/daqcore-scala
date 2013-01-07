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


package daqcore.actors

import akka.actor._
import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future, Promise}

import daqcore.util._
import TypedActorTraits._


trait CloseableTA extends Closeable {
  def isOpen(): Future[Boolean]
}


trait CloseableTAImpl extends Closeable with TypedActorImpl {
  def isOpen(): Future[Boolean] = successful(true)

  def close(): Unit = {
    log.debug("Closing %s".format(selfId))
    selfStop()
  }
}


trait ConditionallyOpenImpl extends CloseableTAImpl {
  val isOpenPromise = Promise[Boolean]()
  val isOpenFuture = isOpenPromise.future

  override def isOpen(): Future[Boolean] = isOpenFuture

  def setIsOpen(status: Boolean): Unit = isOpenPromise success status
  def isOpenOpt: Option[Boolean] = isOpenFuture.value match {
    case None => None
    case Some(Failure(exception)) => throw exception
    case Some(Success(v)) => Some(v)
  }

 override def close(): Unit = {
    if (isOpenOpt.isEmpty) setIsOpen(false)
    super.close()
  }
}
