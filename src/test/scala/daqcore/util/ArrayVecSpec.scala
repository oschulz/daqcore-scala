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
import org.scalatest.Matchers


class ArrayVecSpec extends WordSpec with Matchers {
  "A ArrayVec" when {
    "created from an Seq" should {
      val parent = IndexedSeq(1,2,3,4)
      val seq = ArrayVec(parent: _*)
      "equal it" in {
        assert(seq === parent)
      }
    }

    "built from a mix of traversable types" should {
      val a = List(1, 2, 3, 4)
      val b = ArrayVec(11, 12, 13, 14, 15, 16, 17)
      val c = List(21, 22, 23)

      val builder = ArrayVec.newBuilder[Int]
      builder ++= a
      builder ++= b
      builder ++= c.iterator
      val builderResult = builder.result
      val concatenation = ArrayVec((a ++ b ++ c): _*)

      "equal their direct concatenation" in {
        assert(builderResult === concatenation)
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
    
    "exerciced" should {
      "behave like a Vector" in {
        for (i <- 1 to 10000) {
          def rndFull = util.Random.nextInt
          def rnd(max: Int) = util.Random.nextInt(max)
          
          val n = rnd(16) + 1
          val nTake = rnd(n)
          val nDrop = rnd(n)
          val rev = util.Random.nextBoolean
          val arrayOffs = rnd(n)
          val arrayCpLen = rnd(n)
          
          val ref = Vector.fill(n){rndFull}
          val seq = ArrayVec(ref: _*)
          
          var refIt = if (!rev) ref.iterator else ref.reverseIterator
          var seqIt = if (!rev) seq.iterator else seq.reverseIterator
          
          refIt = refIt.take(nTake).drop(nDrop)
          seqIt = seqIt.take(nTake).drop(nDrop)

          val refArray = Array.ofDim[Int](n)
          val seqArray = Array.ofDim[Int](n)
          refIt.copyToArray(refArray, arrayOffs, arrayCpLen  min (refArray.length - arrayOffs))  // Workaround for
            // bug in scala-2.9.0: generic scala-2.9.0 iterator tries to write past the end of the array on copyToArray
          seqIt.copyToArray(seqArray, arrayOffs, arrayCpLen)
          
          assert(refIt.toList === seqIt.toList)
          assert(refArray.toSeq === seqArray.toSeq)
        }
      }
    }
  }
}
