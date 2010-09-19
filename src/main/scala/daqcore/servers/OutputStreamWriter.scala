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


package daqcore.servers

import java.io.{File, OutputStream, FileOutputStream}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._


class OutputStreamWriter(output: OutputStream) extends CloseableServer with ByteStreamOutput with Closeable {
  override def init() = {
    super.init()
    addResource(output)
  }

  
  protected def srvSend(data: Seq[Byte]): Unit = {
    output.write(data.toArray)
  }

  
  protected def srvFlush(): Unit = {
    output.flush()
  }
 

  override protected def srvClose() = {
    output.flush()
    super.srvClose()
  }
  
  
  override def serve = super.serve orElse {
    case ByteStreamOutput.Send(data) => srvSend(data)
    case ByteStreamOutput.Flush() => srvFlush()
  }
}


object OutputStreamWriter {
  def apply(output: OutputStream): OutputStreamWriter =
    start(new OutputStreamWriter(output))
    
  def apply(file: File, compression: Compression = Uncompressed): OutputStreamWriter =
    OutputStreamWriter(compression.outputStream(file))
}
