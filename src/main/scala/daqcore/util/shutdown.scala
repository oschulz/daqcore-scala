// Copyright (C) 2009-2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.util

object shutdown {
  def prevent : Unit = shutdownLock.readLock.lock
  
  def allow : Unit = shutdownLock.readLock.unlock
  
  def requested : Boolean = hook.isRunning

  private val shutdownLock = new java.util.concurrent.locks.ReentrantReadWriteLock

  private object hook extends Thread {
    private var running = false;
    
    def isRunning = synchronized { running }
    
    override def run() = {
      synchronized { running = true }
      shutdownLock.writeLock.lock
    }
  }

  Runtime.getRuntime.addShutdownHook(hook)
}


object noshutdown {
  def apply[T] (body: => T) : T = {
    shutdown.prevent
    try { body }
    finally { shutdown.allow }
  }
}
