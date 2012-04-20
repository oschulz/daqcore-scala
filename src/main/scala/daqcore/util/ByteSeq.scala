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

import scala.collection.IndexedSeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Queue
import scala.collection.mutable.{Builder}
import scala.collection.mutable.{ArrayOps => ScalaArrayOps}


sealed class ByteSeq private[util] (private val array: Array[Byte]) extends GenericByteSeq with
  collection.immutable.IndexedSeq[Byte] with IndexedSeqLike[Byte, ByteSeq] with
  Serializable
{
  protected[util] final def internalArray = array
  
  @inline final def length = array.length
  @inline final override def size = length

  protected final def newInstance(array: Array[Byte]): ByteSeq =
    new ByteSeq(array)

  protected final def getInstance(it: ByteSeqIterator): ByteSeq =
    if (it isIdenticalTo this.iterator) this
    else newInstance(it.toArray)
  
  final def ++(that: TraversableOnce[Byte]): ByteSeq = {
    def addKnownLength(that: TraversableOnce[Byte], that_length: Int) = {
      val target = Array.ofDim[Byte](this.length + that_length)
      this.copyToArray(target, 0)
      that.copyToArray(target, this.length)
      newInstance(target)
    }
    
    that match {
      case xs: Seq[_] => addKnownLength(that, xs.length)
      case xs: BufferedIterator[_] => {
        val (a, b) = xs.duplicate
        addKnownLength(a, b.length)
      }
      case _ => {
        val bld = ByteSeqBuilder()
        bld ++= this
        bld ++= that
        bld.result
      }
    }
  }
  
  final override def slice(from: Int, until: Int): ByteSeq =
    getInstance(iterator.slice(from, until))
  
  final override def take(n: Int): ByteSeq = slice(0, n)
  final override def takeRight(n: Int): ByteSeq = slice(length - n, length)
  final override def drop(n: Int): ByteSeq = slice(n, length)
  final override def dropRight(n: Int): ByteSeq = slice(0, length - n)

  final override def head: Byte = this(0)
  final override def tail: ByteSeq = this.drop(1)
  final override def last: Byte = this(this.length - 1)
  final override def init: ByteSeq = this.take(this.length - 1)
  
  final override def takeWhile(p: Byte => Boolean) = newInstance(iterator.takeWhile(p).toArray)
  final override def dropWhile(p: Byte => Boolean) = newInstance(iterator.dropWhile(p).toArray)
  final override def span(p: Byte => Boolean): (ByteSeq, ByteSeq) =
    { val (a, b) = iterator.span(p); (getInstance(a), getInstance(b)) }

  final override def splitAt(n: Int): (ByteSeq, ByteSeq) = (take(n), drop(n))
  
  final override def indexWhere(p: Byte => Boolean): Int = iterator.indexWhere(p)
  final override def indexOf[B >: Byte](elem: B): Int = iterator.indexOf(elem)

  final override def reverse = newInstance(reverseIterator.toArray)

  final override def toArray [B >: Byte] (implicit arg0: ClassManifest[B]) = iterator.toArray
  final override def copyToArray [B >: Byte] (xs: Array[B], start: Int, len: Int) = iterator.copyToArray(xs, start, len)
  
  @inline final override def foreach[@specialized U](f: Byte => U): Unit = iterator foreach f

  @inline final def apply(index: Int): Byte = array(index)

  final override def iterator: ByteSeqIterator = ByteSeqIterator(array, 0, array.length)
  final override def reverseIterator: ByteSeqIterator = reverse.iterator
  
  // final override def reverse: ByteSeq

  // final override def map [B, That] (op: (A) => B)(implicit bf: CanBuildFrom[ArrayVec[A], B, That]) : That = {

  final override def foldLeft[@specialized B] (z: B)(op: (B, Byte) => B): B =
    iterator.foldLeft(z)(op)

  final override def foldRight[@specialized B] (z: B)(op: (Byte, B) => B): B =
    iterator.foldRight(z)(op)
  
  final override protected def newBuilder: Builder[Byte, ByteSeq] = ByteSeqBuilder()
  
  final def toByteString = ByteSeq.ByteStringInterop.toByteString(this)
}


object ByteSeq {
  private[ByteSeq] object ByteStringInterop {
    import ByteString.ByteString1

    private object BS1Info {
      val clazz = classOf[ByteString1]
      val arrayField = clazz.getDeclaredField("bytes")
      arrayField.setAccessible(true)
      val startField = clazz.getDeclaredField("startIndex")
      startField.setAccessible(true)
      val endField = clazz.getDeclaredField("length")
      endField.setAccessible(true)
    }
    
    def fromByteString(bs: ByteString): ByteSeq = bs match {
      case bs1: ByteString1 => {
        import BS1Info._
        val array = arrayField.get(bs1).asInstanceOf[Array[Byte]]
        val start = startField.getInt(bs1)
        val length = endField.getInt(bs1)
        if ((start == 0) && (length == array.length)) ByteSeq.wrap(array)
        else ByteSeq.wrap(bs1.toArray)
      }
      case bs => ByteSeq.wrap(bs.toArray)
    }
    
    def toByteString(seq: ByteSeq): ByteString = ByteString() match {
      case bs1: ByteString1 => {
        import BS1Info._
        val array = seq.internalArray
        arrayField.set(bs1, array)
        startField.setInt(bs1, 0)
        endField.setInt(bs1, array.length)
        bs1
      }
      case _ => ByteString(seq.internalArray)
    }
  }


  def apply(values: Byte*): ByteSeq = {
    values match {
      case bs: ByteSeq => bs
      case bs: ByteString => from(bs)
      case vec: ArrayVec[_] => new ByteSeq(vec.asInstanceOf[ArrayVec[Byte]].internalArray)
      case _: Immutable => new ByteSeq(values.toArray)
      case _ => new ByteSeq(values.toArray.clone)
    }
  }

  def apply(string: String): ByteSeq = apply(string, "UTF-8")
  def apply(string: String, charset: String): ByteSeq = wrap(string.getBytes(charset))

  def empty = this.apply()
  
  def wrap(array: Array[Byte]) = new ByteSeq(array)

  def from(bs: ByteString) = ByteStringInterop.fromByteString(bs)

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



sealed class ByteSeqIterator private[util] (private val array: Array[Byte], private var from: Int, private var until: Int) extends
  BufferedIterator[Byte] with GenericByteSeqIterator
{
  protected[util] final def internalArray = array
  protected[util] final def internalFrom = from
  protected[util] final def internalUntil = until

  final def isIdenticalTo(that: ByteSeqIterator): Boolean = {
    ((this.array) eq (that.internalArray)) &&
      ((this.from) == (that.from)) && ((this.until) == (that.until))
  }

  final def isIdenticalTo(that: ArrayIterator[Byte]): Boolean = {
    ((this.array) eq (that.internalArray)) &&
      ((this.from) == (that.internalFrom)) && ((this.until) == (that.internalUntil)) &&
      (that.internalIsReversed == false)
  }

  final def hasNext = from < until

  final def head = array(from)

  final def next() = {
    if (!hasNext) Iterator.empty.next
    else { val i = from; from = from + 1; array(i) }
  }
  
  private final def unsliced = (from == 0) && (until == array.length)
  
  final def len = until - from

  final override def length = { val l = len; drop(len); l }
  final override def size = len
  
  final override def toArray [B >: Byte] (implicit arg0: ClassManifest[B]) : Array[B] = {
    val target = Array.ofDim[B](len)
    copyToArray(target)
    target
  }

  final override def copyToArray [B >: Byte] (xs: Array[B], start: Int, len: Int) : Unit = {
    val n = 0 max ( (xs.length - start) min this.len min len )
    ArrayOps.arrayCopy(this.array, from, xs, start, n)
    this.drop(n)
  }

  protected final def newInstance(array: Array[Byte], from: Int, until: Int): this.type =
    (new ByteSeqIterator(array, from, until)).asInstanceOf[this.type]

  final override def clone: this.type = newInstance(array, from, until)

  final override def duplicate: (ByteSeqIterator, ByteSeqIterator) = (this, clone)
    
  final override def slice (from: Int, until: Int): this.type =
    drop(from).take(until - from)

  final override def take(n: Int): this.type = {
    until = until min (from + (0 max n))
    this
  }

  final override def drop(n: Int): this.type = {
    from = until min (from + (0 max n))
    this
  }
  
  final override def takeWhile(p: Byte => Boolean): this.type = {
    val prev = from
    dropWhile(p)
    until = from; from = prev
    this
  }

  final override def dropWhile(p: Byte => Boolean): this.type = {
    var stop = false
    while (!stop && hasNext) {
      if (p(array(from))) { from = from + 1 } else {stop = true}
    }
    this
  }

  final override def span(p: Byte => Boolean): (ByteSeqIterator, ByteSeqIterator) = {
    val prev = from
    dropWhile(p)
    val that = newInstance(array, prev, from)
    (that, this)
  }

  final override def indexWhere(p: Byte => Boolean): Int = {
    var index = 0
    var found = false
    while (!found && hasNext) if (p(next())) {found = true} else {index += 1}
    if (found) index else -1
  }
  
  final def indexOf(elem: Byte): Int = {
    var index = 0
    var found = false
    while (!found && hasNext) if (elem == next()) {found = true} else {index += 1}
    if (found) index else -1
  }

  final override def indexOf[B >: Byte](elem: B): Int = {
    var index = 0
    var found = false
    while (!found && hasNext) if (elem == next()) {found = true} else {index += 1}
    if (found) index else -1
  }

  protected final def toSharedArray: Array[Byte] = {
    val target = if (unsliced) array else toArray
    drop(len)
    target
  }

  final def toByteSeq: ByteSeq = ByteSeq.wrap(toSharedArray)

  final override def toSeq: ByteSeq = toByteSeq
  
  @inline final override def foreach[@specialized U](f: Byte => U): Unit =
    while (hasNext) f(next())

  final override def foldLeft[@specialized B] (z: B)(op: (B, Byte) => B): B = {
    var acc = z
    while (hasNext) acc = op(acc, next()) 
    acc
  }

  // final override def foldRight[@specialized B] (z: B)(op: (Byte, B) => B): B

  final def splitEvery(n: Int): Stream[ByteSeqIterator] = {
    if (hasNext) { val (a, b) = duplicate; Stream.cons(a.take(n), b.drop(n).splitEvery(n)) }
    else Stream.Empty
  }

  final def sharedWith(that: ByteSeqIterator): Boolean = (this.array == that.array)
}


object ByteSeqIterator {
  private val emptyArray = Array.ofDim[Byte](0)

  def apply(that: ArrayIterator[Byte]): ByteSeqIterator = {
    if (!that.internalIsReversed) new ByteSeqIterator(that.internalArray, that.internalFrom, that.internalUntil)
    else apply(that.toArray)
  }

  protected[util] def apply(array: Array[Byte]): ByteSeqIterator =
    new ByteSeqIterator(array, 0, array.length)

  protected[util] def apply(array: Array[Byte], from: Int, until: Int): ByteSeqIterator =
    new ByteSeqIterator(array, from, until)
  
  def empty: ByteSeqIterator = apply(emptyArray)
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
}


object ByteSeqBuilder {
  private val emptyChunk = Array.empty[Byte]
  private val emptyChunks = Queue.empty[Array[Byte]]

  def apply(defaultChunkSize: Int = 16): ByteSeqBuilder = new ByteSeqBuilder(defaultChunkSize)
}
