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

import scala.annotation._
import akka.actor._, akka.dispatch.Future

import daqcore.util._
import daqcore.actors._


trait GenericInput extends ServerProfile with Logging {
  val inputCompanion: GenericInputCompanion
  import inputCompanion._
  
  protected def tryIn[T](body: => T) = try (body) catch {
    case e: ActorInitializationException => {
      if (srv.isShutdown) throw new java.io.IOException("Input closed")
      else throw e
    }
  }
  
  def toIterator(timeout: Long = defaultTimeout): Iterator[InputData] = new Iterator[InputData] {
    protected var atEnd = false

    protected var nextElem: Option[InputData] = None
    
    protected def recvNext(): Unit = if (!atEnd && (nextElem == None)) tryIn {
      srv.!>(Recv(), timeout)
    }
    
    def hasNext = {
      recvNext()
      !atEnd
    }
    
    def next = {
      recvNext()
      val res = nextElem match {
        case Some(a) => a
        case None => Iterator.empty.next
      }
      nextElem = None
      res
    }
  }

  def recv(timeout: Long = defaultTimeout): InputData = recvF(timeout)get
  
  def recvF(timeout: Long = defaultTimeout): Future[InputData] =
    tryIn { srv.!!>(Recv(), timeout) map { _.data } }
  
  def triggerRecv()(implicit sender: Option[ActorRef]): Unit =
    tryIn { srv.!(Recv())(sender) }

  def clearInput(timeout: Long = defaultTimeout): Unit = {
    @tailrec def clearInputImpl(): Unit = {
      log.trace("Clearing input")
      recvF(timeout).getOpt match {
        case Some(data) => clearInputImpl()
        case None =>
      }
    }
    clearInputImpl()
  }
}


trait GenericInputCompanion {
  type InputData

  case class Recv() extends ActorQuery[Received]
  case class Received(data: InputData)
}



trait GenericOutput extends ServerProfile {
  val outputCompanion: GenericOutputCompanion
  import outputCompanion._

  protected def tryOut[T](body: => T) = try (body) catch {
    case e: ActorInitializationException => {
      if (srv.isShutdown) throw new java.io.IOException("Output closed")
      else throw e
    }
  }

  def send(data: OutputData) : Unit =
    tryOut { srv ! Send(data) }
    
  def flush() : Unit = tryOut { srv ! Flush() }
}


trait GenericOutputCompanion {
  type OutputData

  case class Send(data: OutputData) extends ActorCmd
  
  case class Flush() extends ActorCmd
}
