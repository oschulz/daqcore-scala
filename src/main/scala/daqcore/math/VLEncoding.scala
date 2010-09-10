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


package daqcore.math

import daqcore.util._
import scala.collection.mutable.ArrayBuilder


object VLEncoding {
  def apply(seq: Seq[Int]) : IndexedSeq[Byte] = {
    val inputArray = seq.toArray
    val maxBytesPerValue = (sizeOf[Int] * 8 - 1) / 7 + 1
    val outputArray = Array.ofDim[Byte](seq.size * maxBytesPerValue)
 
    var outputPos = 0;
    for (i <- 0 to (inputArray.size - 1)) {
      var rest = inputArray(i)
      do {
        val newRest = rest >>> 7
        if (newRest == 0) outputArray(outputPos) = (rest & 0x7F).toByte
        else outputArray(outputPos) = ((rest & 0x7F) | 0x80).toByte
        outputPos += 1
        rest = newRest
      } while (rest != 0)
    }
    
    val resultArray = Array.ofDim[Byte](outputPos)
    for (i <- 0 to (outputPos - 1)) resultArray(i) = outputArray(i)
    resultArray.toIISeq
  }

  def unapply(bytes: Seq[Byte]) : Option[IndexedSeq[Int]] = Some {
    val inputArray = bytes.toArray
    val outputArray = Array.ofDim[Int](inputArray.size)
    
    var outputPos = 0;
    var v = 0; var pos = 0;
    for (i <- 0 to (inputArray.size - 1)) {
      val b = inputArray(i)
      v = v | ((b & 0x7f) << pos)
      if ((b & 0x80) == 0) {
        outputArray(outputPos) = v; outputPos += 1
        v = 0; pos = 0
      }
      else pos += 7
    }
    
    val resultArray = Array.ofDim[Int](outputPos)
    for (i <- 0 to (outputPos - 1)) resultArray(i) = outputArray(i)
    resultArray.toIISeq
  }
}


object ZigZagVLEnc {
  def apply(seq: Seq[Int]) = VLEncoding.apply(fast(seq) map ZigZagEnc.encode)
  def unapply(seq: Seq[Byte]) = VLEncoding.unapply(seq) map { fast(_) map ZigZagEnc.decode }
}
