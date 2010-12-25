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
package raw

import java.util.UUID

import daqcore.util._


case class Transient (
  trigPos: Int,
  samples: Seq[Int]
)


case class Event (
  idx: Int,
  run: UUID,
  time: Double,
  systime: Double,
  trig: Seq[Int] = Vector.empty[Int],
  trans: Map[Int, Transient] = Map.empty[Int, Transient]
)


object Event {
  def apply(flat: FlatEvent): Event = {
    val channelIt = flat.trans_samples_ch.iterator
    val trigPosIt = flat.trans_trigPos.iterator
    val nSamplesIt = flat.trans_samples_val_n.iterator

    var transients = Map.empty[Int, raw.Transient]
    var offset = 0

    while(channelIt.hasNext) {
      val channel = channelIt.next
      val trigPos = trigPosIt.next
      val nSamples = nSamplesIt.next
      val samples = (for {v <- flat.trans_samples_val.view.slice(offset, offset + nSamples)} yield v.toInt).toIISeq
      transients = transients + (channel -> raw.Transient(trigPos, samples))
      offset = offset + nSamples
    }

    Event(
      idx = flat.idx,
      run = flat.run,
      time = flat.time,
      systime = flat.systime,
      trig = flat.trig,
      trans = transients
    )
  }
}


case class Events(events: Event*)
