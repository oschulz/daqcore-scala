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
import daqcore.actors._
import daqcore.profiles._
import daqcore.util._
import daqcore.prot.scpi.{SCPIParser, StreamMsgTerm}


class GPIBOverStream(val stream: ByteIO) extends Server with MsgIO {
  gos => 

  protected val defaultTimeout: Long = 10000
  
  case class InputData(bytes: ByteCharSeq)

  object reader extends DaemonActor with Logging {
    var inBuf = ByteCharSeq()

    override def act() = {
      val parser = new SCPIParser
      link(stream.srv)

      loop {
        // trace("inBuf: (" + inBuf.length + ") = [" + inBuf + "]")
        val result = parser.extractTermMsg(inBuf)
        if (result.successful) {
          val msg = result.get
          trace("Complete message available: (" +  msg.length + ") [" + msg + "]")
          readQueue ! InputData(msg)
          inBuf = result.next.source.asInstanceOf[ByteCharSeq].drop(result.next.offset)
        } else {
          // trace("Incomplete message, reading more data")
          val bytes = stream.read()
          inBuf = inBuf ++ bytes
        }
      }
    }
  }

  protected object readQueue extends DaemonActor with Logging {
    import scala.actors.Actor._
    override def act() = loop { react {
      case MsgIO.Read(timeout) => {
        val replyTo = sender
        reactWithin (if (timeout < 0) defaultTimeout else timeout) {
          case InputData(bytes) =>
            trace("Got message: (" + bytes.length + ") = [" + bytes + "]")
            replyTo ! bytes
          case TIMEOUT =>
            trace("Timeout")
            replyTo ! Timeout
        }
      }
    } }
  }
  
  override def init() = {
    link(stream.srv)
    link(reader); reader.startOrRestart()
    link(readQueue); readQueue.startOrRestart()
  }

  def serve = {
    case r: MsgIO.Read => {
      readQueue.forward(r)
    }
    case MsgIO.Write(data) => {
      if ( (!data.isEmpty) && (data(data.length-1) == 0x0A) )
        stream.write(data)
      else stream.write(data ++ StreamMsgTerm)
    }
    case Closeable.Close => {
      stream.close()
      exit('closed)
    }
  }
}


object GPIBOverStream {
  def apply(stream: ByteIO): GPIBOverStream = {
    start(new GPIBOverStream(stream))
  }
}
