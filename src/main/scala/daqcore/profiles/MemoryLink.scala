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

import daqcore.util._
import daqcore.actors._
import daqcore.monads._


trait MemoryReader extends Profile with Closeable {
  def read(address: Long, count: Long): Seq[Byte] =
    readF(address, count)()

  def readF(address: Long, count: Long): Ft[IndexedSeq[Byte]] =
    srv.!!?>(MemoryLink.Read(address, count)) {
      case x: IndexedSeq[_] => x.asInstanceOf[IndexedSeq[Byte]]
    }
}


trait MemoryWriter extends Profile with Closeable {
  def write(address: Long, data: Seq[Byte]) : Unit =
    srv ! MemoryLink.Write(address: Long, data)
  
  def pause(): Unit = srv ! MemoryLink.Pause()
  def sync(): Unit = srv.!?>(MemoryLink.Sync()) { case r: MaybeFail[_] => r() }
}


trait MemoryLink extends MemoryReader with MemoryWriter

object MemoryLink {
  case class Read(address: Long, count: Long)

  case class Write(address: Long, data: Seq[Byte])

  case class Pause()

  case class Sync()
}
