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


trait StreamReader extends ServerProxy with Closeable {
  profile[StreamReader]

  def read(timeout: Long): Future[ByteCharSeq] =
    self.!!& (StreamIO.Read(timeout)) { case x: ByteCharSeq => x }
}


trait StreamWriter extends ServerProxy with Closeable {
  profile[StreamWriter]

  def write(source: Responder[Seq[Byte]]) : Unit =
    self ! StreamIO.Write(source)
    
  def write(source: () => Seq[Byte]) : Unit =
    write(fctResponder(source))

  def write(source: Seq[Byte]) : Unit =
    write(Responder.constant(source))
}


trait StreamIO extends StreamReader with StreamWriter

object StreamIO {
  case class Read(timeout: Long = 0) // Reply: Future[ByteCharSeq]

  case class Write(source: Responder[Seq[Byte]])

  def apply(a: Actor) = new StreamIO { def self = a }
}
