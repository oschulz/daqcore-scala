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

import scala.util.parsing.input._


class ByteCharSeqReader(override val source: ByteCharSeq) extends Reader[Char] {
  import CharSequenceReader._

  override def offset = 0

  def first = if (source.length > 0) source.charAt(0) else EofCh 

  def rest: ByteCharSeqReader = {
    if (source.length > 0)
      new ByteCharSeqReader(source.subSequence(1, source.length))
    else this
  }

  def pos: Position = new OffsetPosition(source, 0)

  def atEnd = source.length <= 0

  override def drop(n: Int): ByteCharSeqReader =
    new ByteCharSeqReader(source.subSequence(n, source.length))
}


object ByteCharSeqReader {
  def apply(source: ByteCharSeq) = new ByteCharSeqReader(source)
}
