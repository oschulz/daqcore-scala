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

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._
import daqcore.prot.scpi.{SCPIParser, StreamMsgTerm}


class GPIBOverStream(val stream: ByteIO) extends Server with RawMsgIO {
  gos => 

  case class InputData(bytes: ByteCharSeq)

  object reader extends Server {
    val replyQueue = collection.mutable.Queue[MsgTarget]()
    val resultQueue = collection.mutable.Queue[ByteCharSeq]()
    
    var inBuf = ByteCharSeq()
    val parser = new SCPIParser

    override protected def init() = {
      super.init()
      link(stream.srv)
      stream.setReceiver(srv, true)
    }
    
    protected def sendReplies(): Unit = {
      while (!replyQueue.isEmpty && !resultQueue.isEmpty)
        replyQueue.dequeue() ! resultQueue.dequeue()
    }
    
    protected def pushResult(bytes: ByteCharSeq): Unit = {
      resultQueue.enqueue(bytes)
      sendReplies()
    }
    
    def serve = {
      case RawMsgInput.Recv() => replyQueue.enqueue(replyTarget)
      case ByteInput.Received(bytes) => {
        inBuf = inBuf ++ bytes
        val result = parser.extractTermMsg(inBuf)
        if (result.successful) {
          val msg = result.get
          trace("Complete message of length %s available: [%s]".format(msg.length, loggable(msg)))
          pushResult(msg)
          inBuf = result.next.source.asInstanceOf[ByteCharSeq].drop(result.next.offset)
        } else {
          // trace("Incomplete message, reading more data")
        }
      }
    }
  }
  
  override def init() = {
    link(stream.srv)
    link(reader); reader.startOrRestart()
  }

  def serve = {
    case r: RawMsgInput.Recv => {
      reader.forward(r)
    }
    case RawMsgOutput.Send(data) => {
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
