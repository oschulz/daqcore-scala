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


package daqcore.util

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers


class SubSeqSpec extends WordSpec with MustMatchers {
  "A SubIdxSeq" when {
    "created from an IndexedSeq" should {
      val parent = IndexedSeq(1,2,3,4)
      val seq = SubIdxSeq(parent)
      "equal it" in {
        assert(seq === parent)
      }
    }

    "sub-sequenced" should {
      val seq = SubIdxSeq(1,2,3,4)
      val sub1 = seq.subSequence(0,2)
      val sub2 = seq.subSequence(2,4)
      
      "share memory between sub-sequences" in {
        assert((sub1 sharedWith sub2) === true)
      }
    }
  }


  "A SubIdxSeq" should {
    "be possible to create from an Array" in {
      val seq = SubIdxSeq(Array(1,2,3,4))

    }

    "be possible to create from values" in {
      val seq = SubIdxSeq(1,2,3,4)
    }
  }
}
