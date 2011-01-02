// Copyright (C) 2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import java.io.{ByteArrayOutputStream}

import java.net.URL


class URLOps(val url: URL) {
  def getBytes(): Array[Byte] = {
    val src = url.openStream()
    try {
      val trg = new ByteArrayOutputStream(8192)
      try {
        val buffer = Array.ofDim[Byte](8192)
        while (
          src.read(buffer) match {
            case nBytes: Int if nBytes > 0 => trg.write(buffer, 0, nBytes); true
            case nBytes: Int => false
          }
        ) {}
        trg.toByteArray
      }
      finally { if (trg != null) src.close() }
    }
    finally {
      if (src != null) src.close()
    }
  }
}
