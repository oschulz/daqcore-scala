// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore
package io

import daqcore.util._


class DataDecoderQueue {
  type DecoderAction = IO.Iteratee[Unit]
  type DecoderActionRef = IO.IterateeRef[Unit]

  protected val dataQueue = collection.mutable.Queue[ByteString]()
  protected val decoderQueue = collection.mutable.Queue[DecoderActionRef]()
  
  protected def processQueues(): Unit = {
    while (!dataQueue.isEmpty && !decoderQueue.isEmpty) {
      val data = dataQueue.dequeue
      val dec = decoderQueue.front
      dec(data)
      dec.value match {
        case (IO.Done(_), bytes) => { // decoding finished
          decoderQueue.dequeue
          // remaining data, push to front of dataQueue
          if (! bytes.isEmpty) bytes +=: dataQueue
        }
        case (cont: IO.Next[_], rest) => {} // decoder needs more data
        case (IO.Failure(cause), rest) => throw cause
      }
    }
  }
  
  def pushData(data: ByteString): Unit = {
    dataQueue.enqueue(data)
    processQueues()
  }
  
  def pushDecoder(decoder: DecoderAction): Unit = {
    decoderQueue.enqueue(IO.IterateeRef(decoder))
    processQueues()
  }
  
  def pendingChunks = dataQueue.size
  def pendingDecoders = decoderQueue.size
}


object DataDecoderQueue {
  def apply() = new DataDecoderQueue
}
