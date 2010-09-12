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

import daqcore.util._
import daqcore.prot.scpi.mnemonics._


class GPIBStreamExtractorSpec extends WordSpec with MustMatchers {
  "An GPIBStreamExtractor" should {
    "work correctly" in {

      for (i <- 1 to 5) {
        val ex = GPIBStreamExtractor()
        val msg = ByteCharSeq("*IDN?;SET:VOLT2:DC 5,\"String with \"\" quote\", 5.5 , 'String with '' quote',#225SomeBinaryDataDummyString\n")
        val nMessages = 5

        var recovered = Seq.empty[Seq[Byte]]
        var remaining = Seq.fill(nMessages)(msg).flatten
        while (remaining.size > 0) {
            val fraction = (2 * msg.size * math.pow(util.Random.nextDouble, 3)).toInt + 1
            val (pkg, rest) = remaining.splitAt(fraction)
            recovered = recovered ++ ex(pkg)
            remaining = rest
        }

        assert(recovered.size === nMessages)
        assert(recovered.view map { _ == msg } reduceLeft { _ && _ })
      }
    }
  }
}
