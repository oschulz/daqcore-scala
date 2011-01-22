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


case class FlatTransients (
  trigPos: Seq[Int],
  samples_ch: Seq[Int],
  samples_val_n: Seq[Int],
  samples_val: ArrayVec[Short]
) {
  def toTransients: Map[Int, Transient] = {
    val channelIt = samples_ch.iterator
    val trigPosIt = trigPos.iterator
    val nSamplesIt = samples_val_n.iterator

    var transients = Map.empty[Int, raw.Transient]
    var offset = 0

    while(channelIt.hasNext) {
      val channel = channelIt.next
      val trigPos = trigPosIt.next
      val nSamples = nSamplesIt.next
      val samples = (for {v <- samples_val.view.slice(offset, offset + nSamples)} yield v.toInt).toArrayVec
      transients = transients + (channel -> raw.Transient(trigPos, samples))
      offset = offset + nSamples
    }
    
    transients
  }
}


object FlatTransients {
  def apply(transients: Map[Int, Transient]): FlatTransients = {
    val transCh = transients.keys.toSeq.sortWith{_ < _}

    FlatTransients(
      samples_ch = for (ch <- transCh) yield ch,
      trigPos = for (ch <- transCh) yield transients(ch).trigPos,
      samples_val_n = for (ch <- transCh) yield transients(ch).samples.size,
      samples_val = (for {ch <- transCh.view; s <-transients(ch).samples.view} yield s.toShort).toArrayVec
    )
  }
}



case class FlatEvent (
  idx: Int,
  run: UUID,
  time: Double,
  systime: Double,
  trig: Seq[Int],
  trans: FlatTransients
) {
  def toEvent: Event = Event(
    idx = idx,
    run = run,
    time = time,
    systime = systime,
    trig = trig,
    trans = trans.toTransients
  )
}


object FlatEvent {
  def apply(event: Event): FlatEvent = {
    val transCh = event.trans.keys.toSeq.sortWith{_ < _}

    FlatEvent(
      run = event.run,
      idx = event.idx,
      time = event.time,
      systime = event.systime,
      trig = event.trig,
      trans = FlatTransients(event.trans)
    )
  }
}
