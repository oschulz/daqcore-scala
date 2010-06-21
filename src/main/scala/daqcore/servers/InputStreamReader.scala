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

import scala.actors._, scala.actors.Actor._
import java.io.{File, InputStream, FileInputStream}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._


class InputStreamReader(input: InputStream) extends EventServer with StreamReader with Closeable {
  val maxChunkSize = 512 * 1024
  
  protected case object ReadNext

  override protected def doAddHandler(handler: EventHandler): Unit = {
    val isFirstHandler = ! hasHandlers
    super.doAddHandler(handler)
    if (isFirstHandler) {
      trace("Triggering first read")
      srv ! ReadNext
    }
  }
  
  protected def doReadNext(): Unit = {
    val avail = input.available
    if (avail > 0) {
      val a = Array.ofDim[Byte](avail min maxChunkSize)
      val count = input.read(a)
      val bytes = ByteCharSeq(if (count < avail) a.take(count) else a)
      trace("Emitting %s bytes".format(bytes.size))
      doEmit(bytes)
      trace("%s remaining handlers".format(nHandlers))
      if (hasHandlers) {
        trace("Triggering next read")
        srv ! ReadNext
      }
    } else {
      doClose()
    }
  }
  
  protected def doClose(): Unit = {
      input.close()
      exit('closed)
  }
  
  override protected def init() = {
    super.init()
  }

  override protected def serve = super.serve orElse {
    case ReadNext => doReadNext()
    case Closeable.Close => doClose()
  }
}


object InputStreamReader {
  def apply(input: InputStream): InputStreamReader =
    start(new InputStreamReader(input))
    
  def apply(file: File): InputStreamReader =
    InputStreamReader(new FileInputStream(file))

  def apply(fileName: String): InputStreamReader =
    InputStreamReader(new File(fileName))
}
