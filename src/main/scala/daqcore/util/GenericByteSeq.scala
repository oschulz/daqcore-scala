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
}


trait GenericByteSeqBuilder extends Builder[Byte, GenericByteSeq] {
}
