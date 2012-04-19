// Copyright (C) 2010-2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.collection.mutable.{Builder}


trait GenericByteSeq extends collection.immutable.IndexedSeq[Byte] {
  // null return values is necessary here to override the return type
  // pure abstract defs are not sufficient for some reason:
  override def iterator: GenericByteSeqIterator = null
  override def reverseIterator: GenericByteSeqIterator = null
}


trait GenericByteSeqIterator extends BufferedIterator[Byte] {
  def getByte()(implicit enc: ValEncoding) = enc.getByte(this)
  def getShort()(implicit enc: ValEncoding) = enc.getShort(this)
  def getInt()(implicit enc: ValEncoding) = enc.getInt(this)
  def getLong()(implicit enc: ValEncoding) = enc.getLong(this)
  def getFloat()(implicit enc: ValEncoding) = enc.getFloat(this)
  def getDouble()(implicit enc: ValEncoding) = enc.getDouble(this)
}


trait GenericByteSeqBuilder extends Builder[Byte, GenericByteSeq] {
  def putByte(x: Byte)(implicit enc: ValEncoding) = enc.putByte(this, x)
  def putShort(x: Short)(implicit enc: ValEncoding) = enc.putShort(this, x)
  def putInt(x: Int)(implicit enc: ValEncoding) = enc.putInt(this, x)
  def putLong(x: Long)(implicit enc: ValEncoding) = enc.putLong(this, x)
  def putFloat(x: Float)(implicit enc: ValEncoding) = enc.putFloat(this, x)
  def putDouble(x: Double)(implicit enc: ValEncoding) = enc.putDouble(this, x)
}
