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


package daqcore
package util

import scala.collection.{TraversableLike, IndexedSeqLike}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{Builder,ArrayBuffer,ArrayBuilder}


sealed class ArrayVec[@specialized A: ClassManifest](array: Array[A]) extends
  collection.immutable.IndexedSeq[A] with IndexedSeqLike[A, ArrayVec[A]] with
  Serializable
{
  final def classMf = classManifest[A]
  
  final protected[util] def getArray = array
  
  @inline final def length = array.length
  @inline final override def size = length
  
  final override def ++[B >: A, That] (that: TraversableOnce[B])(implicit bf: CanBuildFrom[ArrayVec[A], B, That]) : That = {
    that match {
      case thatASeq: ArrayVec[_] => {
        val that = thatASeq.asInstanceOf[ArrayVec[B]]
        val target = Array.ofDim[B](this.length + that.length)(that.classMf)
        this.copyToArray(target, 0)
        that.copyToArray(target, this.length)
        ArrayVec.wrap(target)(that.classMf).asInstanceOf[That]
      }
      case _ => super.++(that)(bf)
    }
  }

  final override def slice(from: Int, until: Int): ArrayVec[A] =
    iterator.slice(from, until).toArrayVec
  
  final override def take(n: Int) = slice(0, n)
  final override def takeRight(n: Int) = slice(length - n, length)
  final override def drop(n: Int) = slice(n, length)
  final override def dropRight(n: Int) = slice(0, length - n)

  final override def head: A = this(0)
  final override def tail: ArrayVec[A] = this.drop(1)
  final override def last: A = this(this.length - 1)
  final override def init: ArrayVec[A] = this.take(this.length - 1)
  
  final override def takeWhile(p: A => Boolean) = iterator.takeWhile(p).toArrayVec
  final override def dropWhile(p: A => Boolean) = iterator.dropWhile(p).toArrayVec
  final override def span(p: A => Boolean) =
    { val (a, b) = iterator.span(p); (a.toArrayVec, b.toArrayVec) }

  final override def splitAt(n: Int) = (take(n), drop(n))
  
  final override def indexWhere(p: A => Boolean): Int = iterator.indexWhere(p)
  override def indexOf[@specialized B >: A](elem: B): Int = iterator.indexOf(elem)

  final override def reverse = reverseIterator.toArrayVec

  final override def toArray [B >: A] (implicit arg0: ClassManifest[B]) = iterator.toArray
  final override def copyToArray [B >: A] (xs: Array[B], start: Int, len: Int) = iterator.copyToArray(xs, start, len)
  
  @inline final override def foreach[@specialized U](f: A => U): Unit = iterator foreach f
    
  @inline final def apply(index: Int): A = array(index)

  final override def iterator = new ArrayIterator(array, 0, array.length, false)
  final override def reverseIterator = new ArrayIterator(array, 0, array.length, true)

  final override def foldLeft[@specialized B] (z: B)(op: (B, A) => B): B =
    iterator.foldLeft(z)(op)

  final override def foldRight[@specialized B] (z: B)(op: (A, B) => B): B =
    iterator.foldRight(z)(op)
  
  final override protected def newBuilder: Builder[A, ArrayVec[A]] =
    (new ArrayBuffer[A]) mapResult { a => val array = a.toArray; new ArrayVec(array) }
  
  final override def map [B, That] (op: (A) => B)(implicit bf: CanBuildFrom[ArrayVec[A], B, That]) : That = {
    def defaultMapImpl = super.map(op)(bf)
  
    if (bf.isInstanceOf[ArrayVec.ArrayVecCanBuildFrom[_, _]]) {
      val bfrom = bf.asInstanceOf[ArrayVec.ArrayVecCanBuildFrom[A, B]]
      val mfA = classManifest[A]
      val mfB = bfrom.mfB.asInstanceOf[ClassManifest[Any]]

      try {
        if (mfA == classManifest[Int]) {
          val f = op.asInstanceOf[Int => _]
          val opt = new IntArrayVecOpt(this.asInstanceOf[ArrayVec[Int]])
          opt.map(f)(mfB).asInstanceOf[That]
        } else if (mfA == classManifest[Long]) {
          val f = op.asInstanceOf[Long => _]
          val opt = new LongArrayVecOpt(this.asInstanceOf[ArrayVec[Long]])
          opt.map(f)(mfB).asInstanceOf[That]
        } else if (mfA == classManifest[Float]) {
          val f = op.asInstanceOf[Float => _]
          val opt = new FloatArrayVecOpt(this.asInstanceOf[ArrayVec[Float]])
          opt.map(f)(mfB).asInstanceOf[That]
        } else if (mfA == classManifest[Double]) {
          val f = op.asInstanceOf[Double => _]
          val opt = new DoubleArrayVecOpt(this.asInstanceOf[ArrayVec[Double]])
          opt.map(f)(mfB).asInstanceOf[That]
        } else defaultMapImpl
      }
      catch { case e: UnsupportedOperationException => defaultMapImpl }
    }
    else defaultMapImpl
  }
}


object ArrayVec {
  def apply[@specialized A: ClassManifest](values: A*) = wrap(values.toArray)
  
  def empty[@specialized A: ClassManifest] = apply()
  
  def wrap[@specialized A: ClassManifest](array: Array[A]): ArrayVec[A] = new ArrayVec(array)

  def fill[@specialized A: ClassManifest](N: Int)(f: => A): ArrayVec[A] = {
    val array = Array.ofDim[A](N)
    for (index <- Range(0, array.length)) array(index) = f
    wrap(array)
  }


  class ArrayVecCanBuildFrom[A, @specialized B: ClassManifest] extends CanBuildFrom[ArrayVec[A], B, ArrayVec[B]] {
    val mfB = classManifest[B]

    final def apply(): Builder[B, ArrayVec[B]] = new ArrayVecBuilder[B](0)

    final def apply(from: ArrayVec[A]): Builder[B, ArrayVec[B]] = new ArrayVecBuilder[B](0)
  }

  implicit def canBuildFrom[A: ClassManifest, B: ClassManifest]: ArrayVecCanBuildFrom[A, B] =
    new ArrayVecCanBuildFrom[A, B]
}



class ArrayVecBuilder[@specialized A: ClassManifest](initialCapacity: Int) extends Builder[A, ArrayVec[A]] {
  val mfA = classManifest[A]

  var array = Array.ofDim[A](initialCapacity)
  
  protected var len = 0
  def length = len

  def capacity = array.length
  
  protected def capacity_=(n: Int) = if (n != capacity) {
    require(n >= len )
    val newArray = Array.ofDim[A](n)
    if (n > 0) java.lang.System.arraycopy(array, 0, newArray, 0, len)
    array = newArray
  }

  def +=(elem: A) = {
    if (len == capacity) capacity = (64 max 2 * capacity)
    array(len) = elem
    len += 1
    this
  }
  
  def clear() = { len = 0; capacity = 0 }

  def result(): ArrayVec[A] = {
    capacity = len
    val res = ArrayVec.wrap(array)
    clear()
    res
  }

  override def sizeHint(size: Int) = {
    capacity = size
  }
  
  override def sizeHint(coll: TraversableLike[_, _], delta: Int = 0) =
    if (coll.isInstanceOf[IndexedSeqLike[_,_]]) sizeHint(coll.size + delta)
}


object ArrayVecBuilder {
  final def apply[@specialized A: ClassManifest](initialCapacity: Int) = new ArrayVecBuilder(initialCapacity)
}


class ArrayVecBoolean(array: Array[Boolean]) extends ArrayVec[Boolean](array) { def this(v: ArrayVec[Boolean]) = this(v.getArray) }
class ArrayVecByte(array: Array[Byte]) extends ArrayVec[Byte](array) { def this(v: ArrayVec[Byte]) = this(v.getArray) }
class ArrayVecChar(array: Array[Char]) extends ArrayVec[Char](array) { def this(v: ArrayVec[Char]) = this(v.getArray) }
class ArrayVecShort(array: Array[Short]) extends ArrayVec[Short](array) { def this(v: ArrayVec[Short]) = this(v.getArray) }
class ArrayVecInt(array: Array[Int]) extends ArrayVec[Int](array) { def this(v: ArrayVec[Int]) = this(v.getArray) }
class ArrayVecLong(array: Array[Long]) extends ArrayVec[Long](array) { def this(v: ArrayVec[Long]) = this(v.getArray) }
class ArrayVecFloat(array: Array[Float]) extends ArrayVec[Float](array) { def this(v: ArrayVec[Float]) = this(v.getArray) }
class ArrayVecDouble(array: Array[Double]) extends ArrayVec[Double](array) { def this(v: ArrayVec[Double]) = this(v.getArray) }
