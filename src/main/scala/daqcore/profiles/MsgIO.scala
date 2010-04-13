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

import scala.actors._

import daqcore.util._
import daqcore.actors._


trait MsgReader extends Profile {
  def readMsg() = as[IndexedSeq[Byte]] { srv !? MsgIO.Read() }

  def readMsgF = as[Future[IndexedSeq[Byte]]] { srv !! MsgIO.Read() }
}


trait MsgWriter extends Profile {
  def writeMsg(contents: IndexedSeq[Byte]) =
    srv ! MsgIO.Write(contents)
}


trait MsgTrigger extends Profile {
  def addHandler(handler: PartialFunction[MsgIO.Event, Unit]) =
    srv ! MsgIO.AddHandler(handler)
}


trait MsgIO extends MsgReader with MsgWriter

object MsgIO {
  //!! Add timeout and optional max read size specification
  case class Read()

  case class Write(contents: IndexedSeq[Byte])

  abstract class Event
  
  case object CanRead extends Event
  
  //!!! Make this a global concept for all Servers/Proxies?
  case class AddHandler(handler: PartialFunction[Event, Unit])
}
