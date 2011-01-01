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


class ByteSeqIteratorOps(it: ByteSeqIterator) {
  import it._

  def getByte()(implicit enc: ValEncoding) = enc.getByte(it)
  def getShort()(implicit enc: ValEncoding) = enc.getShort(it)
  def getInt()(implicit enc: ValEncoding) = enc.getInt(it)
  def getLong()(implicit enc: ValEncoding) = enc.getLong(it)
  def getFloat()(implicit enc: ValEncoding) = enc.getFloat(it)
  def getDouble()(implicit enc: ValEncoding) = enc.getDouble(it)
}


class ByteSeqIteratorCompanion {
  def forChunks(chunks: Seq[Array[Byte]]): ByteSeqIterator =
    ByteSeq.fromChunks(chunks).iterator
}
