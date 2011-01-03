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


class ArrayVecSpec extends WordSpec with MustMatchers {
  "A ArrayVec" when {
    "created from an Seq" should {
      val parent = IndexedSeq(1,2,3,4)
      val seq = ArrayVec(parent: _*)
      "equal it" in {
        assert(seq === parent)
      }
    }

    "sliced" should {
      val seq = ArrayVec(1,2,3,4)
      val sub1 = seq.slice(0,2)
      val sub2 = seq.slice(2,4)
      
      "equal the original after re-assembly" in {
        assert( (sub1 ++ sub2) === seq )
      }
      
      "equal itself when sliced" in {
        assert(sub1 === seq.slice(0,2))
        assert(sub2 === seq.slice(2,4))
      }
    }
  }
}
