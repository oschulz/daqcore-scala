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

import akka.dispatch.Future

import daqcore.util._
import daqcore.actors._


trait MemoryReader extends ServerProfile {
  def read(address: Long, count: Long, timeout: Long = defaultTimeout): ByteSeq =
    readF(address, count, timeout)get

  def readF(address: Long, count: Long, timeout: Long = defaultTimeout): Future[ByteSeq] =
    srv.!!>(MemoryLink.Read(address, count), timeout)
}


trait MemoryWriter extends ServerProfile {
  def write(address: Long, data: ByteSeq) : Unit =
    srv ! MemoryLink.Write(address: Long, data)
  
  def pause(): Unit = srv ! MemoryLink.Pause()

  def sync(timeout: Long = defaultTimeout): Unit = syncF(timeout).get

  def syncF(timeout: Long = defaultTimeout): Future[Unit] =
    srv.!!>(MemoryLink.Sync(), timeout)
}


trait MemoryLink extends MemoryReader with MemoryWriter

object MemoryLink {
  case class Read(address: Long, count: Long) extends ActorQuery[ByteSeq]

  case class Write(address: Long, data: ByteSeq) extends ActorCmd

  case class Pause() extends ActorCmd

  case class Sync() extends ActorQuery[Unit]
}
