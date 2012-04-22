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


package daqcore.io.prot.scpi

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

import daqcore.io._, daqcore.util._
import daqcore.io.prot.scpi._, daqcore.io.prot.scpi.mnemonics._


class SCPIStreamCodecSpec extends WordSpec with MustMatchers {
  "An SCPIStreamCodec" should {
    "work correctly" in {

      import SCPIStreamCodec.{CR, NL, CRNL}

      val codec = SCPIStreamCodec

      val msg = ByteString("*IDN?;SET:VOLT2:DC 5,\"String with \"\" quote\", 5.5 , 'String with '' quote',#225SomeBinaryDataDummyString,7")

      val codec(dec) = msg ++ CRNL
      msg.decodeString(codec.charEncoding)
      dec.decodeString(codec.charEncoding)

      assert (dec === msg ++ CRNL)

      val messages = List(msg ++ NL, msg ++ CRNL, msg ++ NL)
      val input = messages.foldLeft(ByteString.empty) { _ ++ _ }

      val codec(decoded @ _*) = input

      assert (decoded === messages)

    }
  }
}
