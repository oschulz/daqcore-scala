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


object ModbusASCIICodec extends ModbusTransportCodec {
  val frameStart = ByteString(":")
  val frameEnd = ByteString("\r\n")

  private def encFct(out: ByteStringBuilder, msg: ByteString): Unit = {
    val asciiOut = new ASCIIOutputStream(out.asOutputStream);
    val msgArray = msg.toArray
    asciiOut.write(ModbusASCIITransport.FRAME_START);
    asciiOut.write(msgArray, 0, msgArray.length);
    asciiOut.write(ModbusUtil.calculateLRC(msgArray, 0, msgArray.length));
    asciiOut.write(ModbusASCIITransport.FRAME_END);
    asciiOut.flush();
  }

  val enc = encFct(_, _)
  
  val dec = for {
    _ <- Decoder.takeUntil(frameStart, true)
    asciiBody <- Decoder.takeUntil(frameEnd, false)
  } yield {
    val body = {
      val asciiIn = new ASCIIInputStream(asciiBody.iterator.asInputStream)
      val bld = ByteString.newBuilder
      var done = false
      while (!done) {
        val x = asciiIn.read()
        if (x < 0) done = true
        else bld += x.toByte
      }
      bld.result
    }
    if (body.length < 1) throw new IOException("Invalid Modbus ASCII message, missing LRC")
    val (msg, lrc) = (body.init, body.last)
    if (lrc != ModbusUtil.calculateLRC(msg.toArray, 0, msg.length).toByte)
     throw new IOException("Corrupt Modbus ASCII message, LRC check failed")
    else msg
  }
}
