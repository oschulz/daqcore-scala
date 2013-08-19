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

  protected val dataQueue = collection.mutable.Queue[ByteString]()
  protected val decoderQueue = collection.mutable.Queue[DecoderAction]()
  
  protected def processQueues(): Unit = {
    while (!dataQueue.isEmpty && !decoderQueue.isEmpty) {
      val data = dataQueue.dequeue
      val dec = decoderQueue.dequeue
      val (next, rest) = dec(data)
      next match {
        case IO.Done(_) => // Nothing to do
        case cont: IO.Next[_] => cont +=: decoderQueue
        case IO.Failure(cause) => throw cause
      }
      if (!rest.isEmpty) rest +=: dataQueue
    }
  }
  
  def pushData(data: ByteString): Unit = {
    dataQueue.enqueue(data)
    processQueues()
  }
  
  def pushDecoder(decoder: DecoderAction): Unit = {
    decoderQueue.enqueue(decoder)
    processQueues()
  }
  
  def pendingChunks = dataQueue.size
  def pendingDecoders = decoderQueue.size
}


object DataDecoderQueue {
  def apply() = new DataDecoderQueue
}
