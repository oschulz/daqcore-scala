// Copyright (C) 2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>,

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

import org.scalatest.WordSpec
import org.scalatest.Matchers

import daqcore.util._
import daqcore.data.units._


class KeithleyParserSpec extends WordSpec with Matchers {
  "A KeithleyParser" should {
    val parser = KeithleyParser()

    "parse results correctly" in {
      assert( parser.parseResult(ByteString("NDCV+002.2131E+0")) ===
        Result(2.2131~Volt,DC,Result.Normal) )

      assert( parser.parseResult(ByteString("ODCV+002.2131E+0")) ===
        Result(2.2131~Volt,DC,Result.Overflow) )

      assert( parser.parseResult(ByteString("NOHM+002.2131E+0")) ===
        Result(2.2131~Ohm,VAL,Result.Normal) )

      assert( parser.parseResult(ByteString("NACI+002.2131E+0")) ===
        Result(2.2131~Ampere,AC,Result.Normal) )
    }
  }
}
