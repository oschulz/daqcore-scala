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


package daqcore.io.prot.keithley

import daqcore.io._
import daqcore.util._


object KeithleyStreamFramer extends Codec[ByteString, ByteString] {
  val CR = LineCodec.CR
  val LF = LineCodec.LF
  val CRLF = LineCodec.CRLF

  private def encFct(out: ByteStringBuilder, msg: ByteString): Unit = {
    if (msg endsWith CRLF) out ++= msg
    else if (msg endsWith LF) { out ++= msg.dropRight(1) ++= CRLF }
    else { out ++= msg ++= CRLF }
  }

  val enc = encFct(_, _)

  val dec: Decoder[ByteString] = Decoder takeUntil LF map
  	{ msg => if (msg endsWith CR) msg.dropRight(1) else msg }
}
