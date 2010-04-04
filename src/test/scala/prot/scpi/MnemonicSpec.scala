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


package daqcore.prot.scpi

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

import daqcore.util.ByteCSeq


class MnemonicSpec extends WordSpec with MustMatchers {
  val VOLTage = Mnemonic("VOLTage")

  "A SpecMnemonic" should {
    "equal an equal SpecMnemonic" in { Mnemonic("FOObar") === Mnemonic("FOObar") }
  }

  "A RecMnemonic" should {
    "match a SpecMnemonic according to SCPI specs" in {
      expect(false)( VOLTage == RecMnemonic(ByteCSeq("VOLTage")) )
      assert( VOLTage === RecMnemonic(ByteCSeq("VOLTAGE")) )
      assert( VOLTage === RecMnemonic(ByteCSeq("voltage")) )
      assert( VOLTage === RecMnemonic(ByteCSeq("volt")) ) 
      assert( VOLTage === RecMnemonic(ByteCSeq("VOLT")) )
    }
  }
}
