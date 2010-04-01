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


package daqcore.scpi

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

import daqcore.util.ByteCSeq


class MnemonicSpec extends WordSpec with MustMatchers {
  val VOLTage = Mnemonic("VOLTage")
  val IDN = Mnemonic("IDN")

  "Mnemonic()" should {
    "create the right type of Mnemonic" in {
      assert( IDN.isInstanceOf[ShortMnem] )
      assert( VOLTage.isInstanceOf[LongMnem] )
    }
  }

  "A ShortMnem" should {
    "not equal a LongMnem" in { LongMnem("FOO","FOO") != ShortMnem("FOO") }
    "equal an equal ShortMnem" in { Mnemonic("FOO") === Mnemonic("FOO") }
  }

  "A LongMnem" should {
    "not equal a LongMnem" in { LongMnem("FOO","FOO") != ShortMnem("FOO") }
    "equal an equal LongMnem" in { Mnemonic("FOObar") === Mnemonic("FOObar") }
  }

  "A RecMemonic" should {
    "match a ShortMnem according to SCPI specs" in {
      assert( IDN === RecMemonic(ByteCSeq("IDN")) )
      assert( IDN === RecMemonic(ByteCSeq("idn")) )
      expect(false)( IDN == RecMemonic(ByteCSeq("Idn")) )
    }

    "match a LongMnem according to SCPI specs" in {
      expect(false)( VOLTage == RecMemonic(ByteCSeq("VOLTage")) )
      assert( VOLTage === RecMemonic(ByteCSeq("VOLTAGE")) )
      assert( VOLTage === RecMemonic(ByteCSeq("voltage")) )
      assert( VOLTage === RecMemonic(ByteCSeq("volt")) ) 
      assert( VOLTage === RecMemonic(ByteCSeq("VOLT")) )
    }
  }
}
