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

import daqcore.actors._


trait ByteInput extends Profile with MsgSource with Closeable {
  def read()(implicit timeout: TimeoutSpec): Seq[Byte] = readF()(timeout).apply()
  
  def readF()(implicit timeout: TimeoutSpec): Ft[Seq[Byte]] =
    getMsgF()(timeout) map {case ByteIO.Received(bytes) => bytes}
}


trait ByteOutput extends Profile with Closeable {
  def write(data: Seq[Byte]) : Unit =
    srv ! ByteIO.Write(data)
    
  def flush() : Unit = srv ! ByteIO.Flush()
}


trait ByteIO extends ByteInput with ByteOutput

object ByteIO {
  case class Received(bytes: Seq[Byte])

  case class Write(data: Seq[Byte])
  
  case class Flush()
}
