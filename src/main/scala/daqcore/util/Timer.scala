// Copyright (C) 2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>,

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

import java.lang.{System => JSystem}


case class Timer(initNSec: Long = 0, initCount: Long = 0) {
  protected var totalTime: Long = initNSec
  protected var totalCount: Long = initCount

  def nsec: Long = synchronized { totalTime }
  
  def sec: Double = nsec * 1E-9

  def count: Long = synchronized { totalCount }

  def reset(): Unit = synchronized { totalTime = 0; totalCount = 0; }

  def wrap [T, U] (body: PartialFunction[T, U]) : PartialFunction[T, U] = {
    case i if (body isDefinedAt i) => this.apply { body(i) }
  }

  def apply [T] (body : => T) : T = {
    val t1 = JSystem.nanoTime
    val res = body
    val t2 = JSystem.nanoTime
    synchronized {
      totalTime += (t2 - t1)
      totalCount += 1
    }
    res
  }
  
  def +(that: Timer) : Timer = Timer(this.nsec + that.nsec, this.count + that.count)
  
  override def toString = "Timer(sec = %s, count = %s)".format(sec, count)
}


case object Timers extends collection.mutable.HashMap[String, Timer] {
  override def apply(key: String) = this.get(key) match {
    case Some(timer) => timer
    case None => {
      val timer = Timer()
      this += (key -> timer)
      timer
    }
  }
  
  def printTop(n: Int = 20) = this.toSeq.sortWith {(a,b) => a._2.sec >= b._2.sec} take (n) foreach {
    e => println("%s: %s".format(e._1,e._2))
  }
}


trait Profiling {
  protected val timerBaseName = this.getClass.getName
  def prof[T](name: => String)(body : => T) : T = Timers(timerBaseName + "." + name)(body)
  def profilingTimer[T, U](name: => String) : Timer = Timers(timerBaseName + "." + name)
}
