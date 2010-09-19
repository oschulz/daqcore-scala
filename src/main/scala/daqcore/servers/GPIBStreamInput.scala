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

import java.io.{File}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._
import daqcore.prot.scpi.{GPIBStreamExtractor}


trait GPIBStreamInput extends CloseableServer with QueueingServer with RawMsgInput {
  def inputStream: ByteStreamInput
  
  val recvQueue = new ReplyQueue

  override protected def init() = {
    super.init()
    addResource(inputStream)
    inputStream.triggerRecv()
  }
  
  val extractor = GPIBStreamExtractor()
  
  def doHandleInput(bytes: Seq[Byte]) = {
    inputStream.triggerRecv()
    trace("doHandleInput(%s)".format(loggable(bytes)))
    val extracted = extractor(bytes)
    for (msg <- extracted) {
      trace("Complete message of length %s available: [%s]".format(msg.length, loggable(ByteCharSeq(msg: _*))))
      recvQueue.addReply(RawMsgInput.Received(msg)){}
    }
  }
  
  override def srvClose() = {
    super.srvClose()
  }

  override def serve = super.serve orElse {
    case RawMsgInput.Recv() => recvQueue addTarget replyTarget
    case ByteStreamInput.Received(bytes) => doHandleInput(bytes)
    case ByteStreamInput.Closed => {
      recvQueue.addReply(RawMsgInput.Closed){exit('closed)}
    }
  }

}


object GPIBStreamInput {
  def apply(stream: ByteStreamInput): GPIBStreamInput = {
    start(new GPIBStreamInput { val inputStream = stream })
  }

  def apply(file: File, compression: Compression = Uncompressed): GPIBStreamInput = {
    start(new GPIBStreamInput { val inputStream = InputStreamReader(file, compression) })
  }
}
