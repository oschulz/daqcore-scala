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

class DataActionQueue[A] {
  queue => 
  protected val dataQueue = collection.mutable.Queue[A]()
  protected val actionQueue = collection.mutable.Queue[A => Unit]()

  protected def processQueues(): Unit = {
    while (!dataQueue.isEmpty && !actionQueue.isEmpty) {
      val (data, action) = (dataQueue.dequeue(), actionQueue.dequeue())
      action(data)
    }
  }

  def pushData(data: A): Unit = {
    dataQueue.enqueue(data)
    processQueues()
  }

  def pushAction(action: A => Unit): Unit = {
    actionQueue.enqueue(action)
    processQueues()
  }

  def pendingData = dataQueue.size
  def pendingActions = actionQueue.size
}
