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

import scala.collection.immutable.Queue
import scala.collection.{TraversableLike, IndexedSeqLike}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{Builder,ArrayBuffer,ArrayBuilder}


class ByteSeqCompanion {
  def apply(values: Byte*) = wrap(values.toArray)

  def wrap(array: Array[Byte]) = ArrayVec.wrap(array)

  def fromChunks(chunks: Seq[Array[Byte]]): ByteSeq = {
    val length = chunks.map{_.length}.sum
    val dest = Array.ofDim[Byte](length)
    var destPos = 0
    for (chunk <- chunks) {
      ArrayOps.arrayCopy(chunk, 0, dest, destPos, chunk.length)
      destPos += chunk.length
    }
    ArrayVec.wrap(dest)
  }
}



final class ByteSeqBuilder(val chunkSize: Int) extends Builder[Byte, ByteSeq] {
  import ByteSeqBuilder._

  def newChunk = Array.ofDim[Byte](chunkSize)
  
  var chunk = newChunk
  var chunks = emptyChunks
  
  protected var pos = 0

  private def pushChunk(): Unit = if (!chunk.isEmpty) {
      val trimmedChunk =
        if (pos < chunkSize) ArrayOps.copyOf(chunk, pos) else chunk
      chunks = chunks enqueue trimmedChunk
      chunk = newChunk
      pos = 0
  }

  def +=(elem: Byte) = {
    if (pos >= chunkSize) {
      assert(pos == chunkSize)
      pushChunk()
    }
    assert(pos < chunkSize)
    chunk(pos) = elem
    pos += 1
    this
  }
  
  override def ++= (xs: TraversableOnce[Byte]) = {
    xs match {
      case it: ArrayIterator[_] => {
        val xs = it.asInstanceOf[ArrayIterator[Byte]]
        for (x <- it) this += x
      }
      case xs: ArrayVec[_] => this.++=(xs.asInstanceOf[ArrayVec[Byte]].iterator)
      case xs => for (x <- xs) this += x
    }
    this
  }
  
  def fill(N: Int)(f: => Byte): ByteSeqBuilder = {
    for (index <- Range(0, N)) this += f
    this
  }

  def clear() = {
    chunk = newChunk
    chunks = emptyChunks
    pos = 0
  }

  def result(): ByteSeq = {
    pushChunk()
    val res = ByteSeq.fromChunks(chunks)
    clear()
    res
  }

  def putByte(x: Byte)(implicit enc: ValEncoding) = enc.putByte(this, x)
  def putShort(x: Short)(implicit enc: ValEncoding) = enc.putShort(this, x)
  def putInt(x: Int)(implicit enc: ValEncoding) = enc.putInt(this, x)
  def putLong(x: Long)(implicit enc: ValEncoding) = enc.putLong(this, x)
  def putFloat(x: Float)(implicit enc: ValEncoding) = enc.putFloat(this, x)
  def putDouble(x: Double)(implicit enc: ValEncoding) = enc.putDouble(this, x)
}


object ByteSeqBuilder {
  private val emptyChunks = Queue.empty[Array[Byte]]

  def apply(initialCapacity: Int = 1478): ByteSeqBuilder = new ByteSeqBuilder(initialCapacity)
}
