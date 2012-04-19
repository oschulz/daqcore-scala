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

import scala.collection.immutable.Queue
import scala.collection.mutable.{Builder}
import scala.collection.mutable.{ArrayOps => ScalaArrayOps}

class ByteSeq(array: Array[Byte]) extends ArrayVec[Byte](array) with GenericByteSeq {
  final override def iterator: ByteSeqIterator = ByteSeqIterator(array, 0, array.length, false)
  final override def reverseIterator: ByteSeqIterator = ByteSeqIterator(array, 0, array.length, true)

  final override protected def newInstance(array: Array[Byte]): this.type =
    new ByteSeq(array).asInstanceOf[this.type]

  final override def span(p: Byte => Boolean): (ByteSeq, ByteSeq) =
    { val (a, b) = iterator.span(p); (getInstance(a), getInstance(b)) }

  final override def splitAt(n: Int): (ByteSeq, ByteSeq) = (take(n), drop(n))

  final override def reverse = new ByteSeq(super.reverse.internalArray)
}


object ByteSeq {
  def apply(values: Byte*): ByteSeq = {
    values match {
      case seq: ByteSeq => seq
      case vec: ArrayVec[_] => new ByteSeq(vec.asInstanceOf[ArrayVec[Byte]].internalArray)
      case x => apply(ArrayVec(x: _*): _*)
    }
  }

  def apply(string: String): ByteSeq = apply(string, "UTF-8")
  def apply(string: String, charset: String): ByteSeq = wrap(string.getBytes(charset))

  def empty = this.apply()
  
  def wrap(array: Array[Byte]) = new ByteSeq(array)

  def fromChunks(chunks: Seq[Array[Byte]]): ByteSeq = {
    val length = chunks.map{_.length}.sum
    val dest = Array.ofDim[Byte](length)
    var destPos = 0
    for (chunk <- chunks) {
      ArrayOps.arrayCopy(chunk, 0, dest, destPos, chunk.length)
      destPos += chunk.length
    }
    wrap(dest)
  }
}



class ByteSeqIterator(private val array: Array[Byte], private var from: Int, private var until: Int, private var isReversed: Boolean)
  extends ArrayIterator[Byte](array, from, until, isReversed) with GenericByteSeqIterator
{
  override protected def newInstance(array: Array[Byte], from: Int, until: Int, isReversed: Boolean): this.type =
    (new ByteSeqIterator(array, from, until, isReversed)).asInstanceOf[this.type]

  def this(that: ArrayIterator[Byte]) =
    this(that.internalArray, that.internalFrom, that.internalUntil, that.internalIsReversed)

  override def duplicate: (ByteSeqIterator, ByteSeqIterator) = (this, clone)

  override def span(p: Byte => Boolean): (ByteSeqIterator, ByteSeqIterator) = {
    val (a, b) = span(p)
    (new ByteSeqIterator(a), new ByteSeqIterator(b))
  }

  def toByteSeq: ByteSeq = ByteSeq.wrap(toSharedArray)

  override def toSeq: ByteSeq = toByteSeq
}


object ByteSeqIterator {
  def apply(that: ArrayIterator[Byte]): ByteSeqIterator = new ByteSeqIterator(that)

  def apply(array: Array[Byte], from: Int, until: Int, reverse: Boolean): ByteSeqIterator =
    new ByteSeqIterator(array, from, until, reverse)

  def forArray(array: Array[Byte]): ByteSeqIterator =
    new ByteSeqIterator(array, 0, array.length, false)

  def forArrayRange(array: Array[Byte], from: Int, until: Int): ByteSeqIterator =
    new ByteSeqIterator(array, from, until, false)
  
  def empty: ByteSeqIterator =
    forArray(Array.empty[Byte])

  //def forChunks(chunks: Seq[Array[Byte]]): ByteSeqIterator =
    //ByteSeq.fromChunks(chunks).iterator
}



class ByteSeqBuilder(val defaultChunkSize: Int) extends
  GenericByteSeqBuilder with Builder[Byte, ByteSeq]
{
  import ByteSeqBuilder._

 
  require(defaultChunkSize > 0)

  private var chunk = emptyChunk
  private var chunkCapacity = 0
  private var pos = 0
  private var chunks = emptyChunks
  
  private def clearChunk(): Unit = {
    chunk = emptyChunk
    chunkCapacity = 0
    pos = 0
  }
  
  private def pushChunk(): Unit = if (pos > 0) {
    val trimmedChunk =
      if (pos < chunkCapacity) ArrayOps.copyOf(chunk, pos) else chunk
    chunks = chunks enqueue trimmedChunk
    clearChunk()
  }

  def newChunk(): Unit = {
    pushChunk()
    chunk = Array.ofDim[Byte](defaultChunkSize)
    chunkCapacity = chunk.size
  }

  def +=(elem: Byte) = {
    if (pos >= chunkCapacity) {
      assert(pos == chunkCapacity)
      newChunk()
    }
    chunk(pos) = elem
    pos += 1
    this
  }
  

  override def ++= (xs: TraversableOnce[Byte]) = {
    xs match {
      case wrapped: ScalaArrayOps[_] => {
        val xs = wrapped.asInstanceOf[ScalaArrayOps[Byte]].repr
        pushChunk()
        chunk = xs.toArray
        pos = xs.length
        pushChunk()
      }
      case it: ArrayIterator[_] => {
        val xs = it.asInstanceOf[ArrayIterator[Byte]]
        val len = xs.len
        if (len <= chunkCapacity - pos) {
          xs.copyToArray(chunk, pos)
          assert(!xs.hasNext)
          pos += len
        }
        else this.++=(xs.toArray)
      }
      case vec: ArrayVec[_] => this.++=(vec.asInstanceOf[ArrayVec[Byte]].iterator)
      case seq: IndexedSeq[_] => {
        val xs = seq.asInstanceOf[IndexedSeq[Byte]]
        if (xs.length <= chunkCapacity - pos) {
          xs.copyToArray(chunk, pos)
          pos += xs.length
        }
        else this.++=(xs.toArray)
      }
      case xs => for (x <- xs) this += x
    }
    this
  }
  
  def fill(N: Int)(f: => Byte): ByteSeqBuilder = {
    for (index <- Range(0, N)) this += f
    this
  }

  def clear() = {
    clearChunk()
    chunks = emptyChunks
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
  private val emptyChunk = Array.empty[Byte]
  private val emptyChunks = Queue.empty[Array[Byte]]

  def apply(defaultChunkSize: Int = 1478): ByteSeqBuilder = new ByteSeqBuilder(defaultChunkSize)
}
