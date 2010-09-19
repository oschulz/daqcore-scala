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

import java.io.{File, InputStream, FileInputStream}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._


class InputStreamReader(input: InputStream) extends CloseableServer with ByteStreamInput with Closeable {
  val maxChunkSize = 512 * 1024

  override def init() = {
    super.init()
    addResource(input)
  }
  
  protected def srvRecv(): Unit = {
    val avail = input.available
    if (avail > 0) {
      val a = Array.ofDim[Byte](avail min maxChunkSize)
      val count = input.read(a)
      val bytes = if (count < avail) a.take(count) else a
      reply(ByteStreamInput.Received(bytes))
    } else {
      reply(ByteStreamInput.Closed)
      srvClose()
    }
  }
  
  override def serve = super.serve orElse {
    case ByteStreamInput.Recv() => srvRecv()
  }
}


object InputStreamReader {
  def apply(input: InputStream): InputStreamReader =
    start(new InputStreamReader(input))
    
  def apply(file: File, compression: Compression = Uncompressed): InputStreamReader =
    InputStreamReader(compression.inputStream(file))
}
