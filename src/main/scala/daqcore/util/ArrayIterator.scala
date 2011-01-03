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


final class ArrayIterator[@specialized A: ClassManifest](private val array: Array[A], private var from: Int, private var until: Int, private var isReversed: Boolean) extends
  BufferedIterator[A]
{
  def hasNext = from < until

  def head = if (!isReversed) array(from) else array(until - 1)

  def next() = {
    if (!hasNext) Iterator.empty.next
    if (!isReversed) { val i = from; from = from + 1; array(i) }
    else { val i = until - 1; until = until - 1; array(i) }
  }
  
  private def unsliced = (from == 0) && (until == array.length) && (!isReversed)
  
  override def length = until - from
  override def size = length
  
  override def toArray [B >: A] (implicit arg0: ClassManifest[B]) : Array[B] = {
    val target = Array.ofDim[B](length)
    copyToArray(target)
    target
  }

  override def copyToArray [B >: A] (xs: Array[B], start: Int, len: Int) : Unit = {
    val n = 0 max ( (xs.length - start) min this.length min len )
    if (!isReversed) ArrayOps.arrayCopy(this.array, from, xs, start, n)
    else ArrayOps.reverseArrayCopy(this.array, until-n, xs, start, n)
    this.drop(n)
  }

  override def duplicate: (ArrayIterator[A], ArrayIterator[A]) =
    (this, new ArrayIterator(array, from, until, isReversed))
    
  def reverse: Iterator[A] = {
    isReversed = ! isReversed
    this
  }

  override def slice (from: Int, until: Int): ArrayIterator[A] =
    drop(from).take(until - from)

  override def take(n: Int): ArrayIterator[A] = {
    if (!isReversed) until = until min (from + (0 max n))
    else from = from max (until - (0 max n))
    this
  }

  override def drop(n: Int): ArrayIterator[A] = {
    if (!isReversed) from = until min (from + (0 max n))
    else until = from max (until - (0 max n))
    this
  }
  
  override def takeWhile(p: A => Boolean): ArrayIterator[A] = {
    val prev = if (!isReversed) from else until
    dropWhile(p)
    if (!isReversed) { until = from; from = prev }
    else { from = until; until = prev }
    this
  }

  override def dropWhile(p: A => Boolean): ArrayIterator[A] = {
    var stop = false
    if (!isReversed) while (!stop && hasNext) {
      if (p(array(from))) { from = from + 1 } else {stop = true}
    } else while (!stop && hasNext) {
      if (p(array(until - 1))) { until = until - 1 } else {stop = true}
    }
    this
  }

  def span(p: A => Boolean): (ArrayIterator[A], ArrayIterator[A]) = {
    val prev = if (!isReversed) from else until
    dropWhile(p)
    val that = {
      if (!isReversed) new ArrayIterator(array, prev, from, isReversed)
      else new ArrayIterator(array, until, prev, isReversed)
    }
    (that, this)
  }

  override def indexWhere(p: A => Boolean): Int = {
    var index = 0
    var found = false
    while (!found && hasNext) if (p(next())) {found = true} else {index += 1}
    if (found) index else -1
  }
  
  override def indexOf[@specialized B >: A](elem: B): Int = {
    var index = 0
    var found = false
    while (!found && hasNext) if (elem == next()) {found = true} else {index += 1}
    if (found) index else -1
  }

  def toArrayVec: ArrayVec[A] = {
    val target = if (unsliced) array else toArray
    drop(length)
    ArrayVec.wrap(target)
  }

  override def toSeq: ArrayVec[A] = toArrayVec
  
  @inline override final def foreach[@specialized U](f: A => U): Unit =
    while (hasNext) f(next())

  override def foldLeft[@specialized B] (z: B)(op: (B, A) => B): B = {
    var acc = z
    while (hasNext) acc = op(acc, next()) 
    acc
  }

  override def foldRight[@specialized B] (z: B)(op: (A, B) => B): B = {
    reverse
    var acc = z
    while (hasNext) acc = op(next(), acc) 
    acc
  }

  def sharedWith(that: ArrayIterator[_]): Boolean = this.array == that.array
}


object ArrayIterator {
  def forArray[@specialized A: ClassManifest](array: Array[A]): ArrayIterator[A] =
    new ArrayIterator(array, 0, array.length, false)
}
