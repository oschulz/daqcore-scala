// Copyright (C) 2010-2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import java.io.{OutputStream, File, FileOutputStream}

import akka.actor._
import scala.concurrent.{Future, Promise}

import daqcore.util._
import daqcore.actors._, daqcore.actors.TypedActorTraits._


trait OutputStreamWriter extends ByteStreamOutput with CloseableTA {}


object OutputStreamWriter {
  def openOutputStream(file: File): OutputStream = {
    if (file.getPath() == "-") System.out
    else new FileOutputStream(file)
  }

  def apply(stream: OutputStream, name: String)(implicit rf: ActorRefFactory): OutputStreamWriter =
    typedActorOf[OutputStreamWriter](new WriterImpl(stream), name)
  
  def apply(file: File, name: String)(implicit rf: ActorRefFactory): OutputStreamWriter = {
    typedActorOf[OutputStreamWriter](new WriterImpl(openOutputStream(file), file.getPath()), name)
  }

  def apply(finalFileName: String = "", tmpFileName: String, name: String = "")(implicit rf: ActorRefFactory): OutputStreamWriter = {
    typedActorOf[OutputStreamWriter](new WriterImpl(openOutputStream(new File(tmpFileName)), finalFileName, tmpFileName), name)
  }

  class WriterImpl(protected val stream: OutputStream, finalFileName: String = "", tmpFileName: String = "") extends OutputStreamWriter
    with ByteStreamOutputImpl
  {
    atCleanup {
      if (!((stream eq System.out) || (stream eq System.err))) stream.close()
      if (!tmpFileName.isEmpty) {
        require(!finalFileName.isEmpty)
        if ((new File(tmpFileName)).renameTo(new File(finalFileName)) != true)
          throw new RuntimeException(s"Couldn't rename ${tmpFileName} to ${finalFileName}")
      }
    }
    
    def flush(): Unit = {
      val bytes = outputQueue.result
      if (! bytes.isEmpty) {
        stream.write(bytes.toArray)
        outputQueue.clear
        stream.flush()
      }
    }
  }
}
