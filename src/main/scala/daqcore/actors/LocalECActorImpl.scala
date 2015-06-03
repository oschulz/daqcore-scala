// Copyright (C) 2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.concurrent.{ExecutionContext, Future}
import akka.actor.ActorRef


trait LocalECActorImpl extends AbstractActorImpl {
  import LocalECActorImpl._

  protected val localExecContext = new LocalEC(selfRef)

  protected def localExec[T](futureExec: (ExecutionContext => Future[T])): Future[T] =
    monitor(futureExec(localExecContext))

  protected def monitor[T](future: Future[T]) = {
    future.onFailure(throwAll)(localExecContext)
    future
  }

  protected def recvLocalExec: PartialFunction[Any, Unit] = {
    case msg: LocalExecMsg => msg match {
      case LocalExecRun(runnable) =>
        // log.trace(s"Actor-local execution of $runnable")
        runnable.run()
      case LocalExecFailed(cause) =>
        throw cause
    }
  }
}


object LocalECActorImpl {
  protected sealed trait LocalExecMsg
  protected case class LocalExecRun(runnable: Runnable) extends LocalExecMsg
  protected case class LocalExecFailed(cause: Throwable) extends LocalExecMsg

  protected class LocalEC(actor: ActorRef) extends ExecutionContext {
    def execute(runnable: Runnable) = actor ! LocalExecRun(runnable)
    def reportFailure(cause: Throwable) = actor ! LocalExecFailed(cause)
  }

  protected def throwAll: PartialFunction[Throwable, Unit] = {case cause => throw cause}
}



trait LocalECUntypedActorImpl extends UntypedActorImpl with LocalECActorImpl {
  override def receive = extend(super.receive)(recvLocalExec)
}


trait LocalECTypedActorImpl extends TypedActorImpl with LocalECActorImpl {
  override def receive = extend(super.receive)(recvLocalExec)
}