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


package daqcore.io.prot.modbus

import java.nio.{ByteBuffer, ByteOrder => NIOByteOrder}

import daqcore.util._


class ModbusShortCoding(val nioByteOrder: NIOByteOrder) {
  object BooleanEnc {
    def apply(values: Boolean*): Seq[Short] = 
	  values map { v => if (v) 1.toShort else 0.toShort }
    
    def unapplySeq(values: Seq[Short]): Option[Seq[Boolean]] =
	  Some( values map { v => if (v > 0) true else false} )
  }

	  
  object IntEnc {
    val sizeRatio = sizeOf[Int]/sizeOf[Short]

    def apply(values: Int*): Seq[Short] = {
      val buf = ByteBuffer.allocate(values.length * sizeOf[Int])
      buf.order(nioByteOrder); values foreach buf.putInt; buf.flip
      val trg = values.genericBuilder[Short]
      val n = values.length * sizeRatio
      trg.sizeHint(n); for (i <- 1 to n) trg += buf.getShort; trg.result
    }
    
    def unapplySeq(values: Seq[Short]): Option[Seq[Int]] = {
      if (values.length % sizeRatio != 0) None
      else {
        val buf = ByteBuffer.allocate(values.length * sizeOf[Short])
        buf.order(nioByteOrder); values foreach buf.putShort; buf.flip
        val trg = values.genericBuilder[Int]
        val n = values.length / sizeRatio
        trg.sizeHint(n); for (i <- 1 to n) trg += buf.getInt; Some(trg.result)
      }
    }
  }


  object FloatEnc {
    val sizeRatio = sizeOf[Float]/sizeOf[Short]

    def apply(values: Float*): Seq[Short] = {
      val buf = ByteBuffer.allocate(values.length * sizeOf[Float])
      buf.order(nioByteOrder); values foreach buf.putFloat; buf.flip
      val trg = values.genericBuilder[Short]
      val n = values.length * sizeRatio
      trg.sizeHint(n); for (i <- 1 to n) trg += buf.getShort; trg.result
    }
    
    def unapplySeq(values: Seq[Short]): Option[Seq[Float]] = {
      if (values.length % sizeRatio != 0) None
      else {
        val buf = ByteBuffer.allocate(values.length * sizeOf[Short])
        buf.order(nioByteOrder); values foreach buf.putShort; buf.flip
        val trg = values.genericBuilder[Float]
        val n = values.length / sizeRatio
        trg.sizeHint(n); for (i <- 1 to n) trg += buf.getFloat; Some(trg.result)
      }
    }
  }
}



object BigEndianShortCoding extends ModbusShortCoding(BigEndian.nioByteOrder)


object LittleEndianShortCoding extends ModbusShortCoding(LittleEndian.nioByteOrder)
