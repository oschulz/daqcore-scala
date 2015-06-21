// Copyright (C) 2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>,

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

import scala.concurrent.duration._


case class NanoTimeWindow(from: NanoTime, to: NanoTime) {
  def time: NanoTime = (to + from) / 2

  def duration: Duration =
    (to - from).nanoseconds

  def isPointLike: Boolean = (to == from)
}

case object NanoTimeWindow {
	def apply(time: NanoTime): NanoTimeWindow = NanoTimeWindow(time, time)
}
