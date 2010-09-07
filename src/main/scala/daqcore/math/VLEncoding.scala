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
    val builder = ArrayBuilder.make[Byte]
    val maxBytesPerValue = (sizeOf[Int] * 8 - 1) / 7 + 1
    builder.sizeHint(seq.size * maxBytesPerValue)
    
    for {v <- seq} {
      var rest = v
      do {
        val newRest = rest >>> 7
        if (newRest == 0) builder += (rest & 0x7F).toByte
        else builder += ((rest & 0x7F) | 0x80).toByte
        rest = newRest
      } while (rest != 0)
    }
    
    builder.result.toSeq.asInstanceOf[IndexedSeq[Byte]]
  }

  def unapply(bytes: Seq[Byte]) : Option[IndexedSeq[Int]] = Some {
    val builder = ArrayBuilder.make[Int]
    builder.sizeHint(bytes.size)
    
    var v = 0; var pos = 0;
    for { b <- bytes } {
      v = v | ((b & 0x7f) << pos)
      if ((b & 0x80) == 0) { builder += v; v = 0; pos = 0; }
      else pos += 7
    }
    
    builder.result.toSeq.asInstanceOf[IndexedSeq[Int]]
  }
}


object ZigZagVLEnc {
  def apply(seq: Seq[Int]) = VLEncoding.apply(seq.view map ZigZagEnc.encode)
  def unapply(seq: Seq[Byte]) = VLEncoding.unapply(seq) map { _ map ZigZagEnc.decode }
}
