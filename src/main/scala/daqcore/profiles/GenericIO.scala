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


package daqcore.profiles

import scala.annotation._

import daqcore.util._
import daqcore.actors._


trait GenericInput extends Profile with Closeable {
  val inputCompanion: GenericInputCompanion
  import inputCompanion._

  def recv()(implicit timeout: TimeoutSpec): InputData = recvF()(timeout).apply()
  
  def recvF()(implicit timeout: TimeoutSpec): Ft[InputData] =
    srv.!!?(Recv())(timeout) map
      {case Received(data) => data}
  
  def triggerRecv(): Unit = srv ! Recv()

  def clearInput(timeout: Long): Unit = {
    @tailrec def clearInputImpl(): Unit = {
      trace("Clearing input")
      recvF()(SomeTimeout(timeout)).get match {
        case Some(data) => clearInputImpl()
        case None =>
      }
    }
    clearInputImpl()
  }
}


trait GenericInputCompanion {
  type InputData

  case class Recv()
  case class Received(data: InputData)
  case object Closed
}



trait GenericOutput extends Profile with Closeable {
  val outputCompanion: GenericOutputCompanion
  import outputCompanion._

  def send(data: OutputData) : Unit =
    srv ! Send(data)
    
  def flush() : Unit = srv ! Flush()
}


trait GenericOutputCompanion {
  type OutputData

  case class Send(data: OutputData)
  
  case class Flush()
}
