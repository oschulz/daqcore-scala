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

import java.io.{IOException}

import net.wimpi.modbus.Modbus
import net.wimpi.modbus.msg._
import net.wimpi.modbus.io._
import net.wimpi.modbus.util._

import daqcore.io._
import daqcore.util._


object ModbusTCPCodec extends ModbusTransportCodec {
  implicit val byteOrder = BigEndian.nioByteOrder

  private def encFct(out: ByteStringBuilder, msg: ByteString): Unit = {
    out.putShort(0) // Transaction Identifier, zero for now
    out.putShort(0) // Protocol Identifier, zero for now
    out.putShort(msg.length.toShort) // Length field
    out ++= msg // Content: Address, Function, Data
  }

  val enc = encFct(_, _)
  
  val dec = for {
    header <- IO.take(6)
    headerIterator = header.iterator
    transactionId = headerIterator.getShort.toInt & 0xffff
    protocolId = headerIterator.getShort.toInt & 0xffff
    contentLen = headerIterator.getShort.toInt & 0xffff
    content <- IO.take(contentLen)
  } yield {
	  if (transactionId != 0) throw new RuntimeException("Unexpected Modbus TCP transaction ID")
	  if (protocolId != 0) throw new RuntimeException("Unexpected Modbus TCP protocol ID")
	  content
  }
}
