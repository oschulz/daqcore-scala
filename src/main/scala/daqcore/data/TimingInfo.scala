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


package daqcore.data

import java.util.UUID
import scala.concurrent.duration._

import daqcore.util._
import java.io.File


trait StartInfo {
  def startTime: Double

  // never negative
  def currentDuration: Double = (currentTime - startTime) max 0.0
}


trait StopInfo {
  def duration: Double
}


trait TimingInfo {
  def startTime: Double
  def duration: Double
}



case class EventStart(startTime: Double = currentTime) extends StartInfo {
  def currentTiming: EventTiming = EventTiming(startTime, currentDuration)
  def timing(stop: EventStop): EventTiming = EventTiming(startTime, stop.duration)
}

case class EventTiming(startTime: Double, duration: Double) extends TimingInfo

case class EventStop(duration: Double) extends StopInfo



case class RunInfo (
  idx: Int,
  uuid: UUID = UUID.randomUUID(),
  startTime: Double,
  duration: Double
) extends TimingInfo {
  def this(start: RunStart, stop: RunStop) = this(
    idx = start.idx,
    uuid = start.uuid,
    startTime = start.startTime,
    duration = stop.duration
  )
}


case class RunStart (
  uuid: UUID = UUID.randomUUID(),
  idx: Int = 0,
  startTime: Double = currentTime.toLong.toDouble
) extends StartInfo {
  def currentTiming: RunInfo = new RunInfo(this, RunStop(currentDuration))
  
  def fileName(prefix: String, extension: String): String = {
    val timeStamp = FileNameTimeStamp(startTime)
    val uuidStamp = uuid.toString.take(8)
    "%s_%s_%s.%s".format(prefix, timeStamp, uuidStamp, extension)
  }

  def file(prefix: File, extension: String): File =
    new File(fileName(prefix.getPath, extension))
}


case class RunStop(duration: Double) extends StopInfo
