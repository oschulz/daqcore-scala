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

import scala.reflect.{ClassTag, classTag}
import scala.collection.mutable.ArrayBuilder


class ArrayOps[A: ClassTag](array: Array[A]) {
  def toIISeq = array.toSeq.asInstanceOf[IndexedSeq[A]]

  def toArrayVec: ArrayVec[A] = ArrayVec.wrap(array)
}


object ArrayOps {
  def copyOf[A](src: Array[A], newLength: Int): Array[A] = {
    val ct = ClassTag(src.getClass.getComponentType)
    val dest = ct.newArray(newLength).asInstanceOf[Array[A]]
    arrayCopy(src, 0, dest, 0, src.length min dest.length)
    dest
  }

  def arrayCopy[A, B >: A](src: Array[A], srcPos: Int, dest: Array[B], destPos: Int, length: Int): Unit = {
    java.lang.System.arraycopy(src, srcPos, dest, destPos, length)
  }

  def reverseArrayCopy[A, B >: A](src: Array[A], srcPos: Int, dest: Array[B], destPos: Int, length: Int): Unit = {
    if ( (srcPos < 0) || (destPos < 0) || (length < 0) || (srcPos+length > src.length) || (destPos+length > dest.length) )
      throw new IndexOutOfBoundsException
    
    val from = if (src eq dest) copyOf(src, src.length) else src
    val to = dest
    val (fromClass, toClass) = (from.getClass, to.getClass)

    if ((fromClass == classOf[Array[Byte]]) && (toClass == classOf[Array[Byte]])) {
      val (source, target) = (from.asInstanceOf[Array[Byte]], to.asInstanceOf[Array[Byte]])
      val srcLast = srcPos + length - 1
      for (i <- Range(0, length)) { target(destPos + i) = source(srcLast - i) }
      target.asInstanceOf[Array[A]]
    } else if ((fromClass == classOf[Array[Char]]) && (toClass == classOf[Array[Char]])) {
      val (source, target) = (from.asInstanceOf[Array[Char]], to.asInstanceOf[Array[Char]])
      val srcLast = srcPos + length - 1
      for (i <- Range(0, length)) { target(destPos + i) = source(srcLast - i) }
      target.asInstanceOf[Array[A]]
    } else if ((fromClass == classOf[Array[Short]]) && (toClass == classOf[Array[Short]])) {
      val (source, target) = (from.asInstanceOf[Array[Short]], to.asInstanceOf[Array[Short]])
      val srcLast = srcPos + length - 1
      for (i <- Range(0, length)) { target(destPos + i) = source(srcLast - i) }
      target.asInstanceOf[Array[A]]
    } else if ((fromClass == classOf[Array[Int]]) && (toClass == classOf[Array[Int]])) {
      val (source, target) = (from.asInstanceOf[Array[Int]], to.asInstanceOf[Array[Int]])
      val srcLast = srcPos + length - 1
      for (i <- Range(0, length)) { target(destPos + i) = source(srcLast - i) }
      target.asInstanceOf[Array[A]]
    } else if ((fromClass == classOf[Array[Long]]) && (toClass == classOf[Array[Long]])) {
      val (source, target) = (from.asInstanceOf[Array[Long]], to.asInstanceOf[Array[Long]])
      val srcLast = srcPos + length - 1
      for (i <- Range(0, length)) { target(destPos + i) = source(srcLast - i) }
      target.asInstanceOf[Array[A]]
    } else if ((fromClass == classOf[Array[Float]]) && (toClass == classOf[Array[Float]])) {
      val (source, target) = (from.asInstanceOf[Array[Float]], to.asInstanceOf[Array[Float]])
      val srcLast = srcPos + length - 1
      for (i <- Range(0, length)) { target(destPos + i) = source(srcLast - i) }
      target.asInstanceOf[Array[A]]
    } else if ((fromClass == classOf[Array[Double]]) && (toClass == classOf[Array[Double]])) {
      val (source, target) = (from.asInstanceOf[Array[Double]], to.asInstanceOf[Array[Double]])
      val srcLast = srcPos + length - 1
      for (i <- Range(0, length)) { target(destPos + i) = source(srcLast - i) }
      target.asInstanceOf[Array[A]]
    } else if ((fromClass == classOf[Array[Unit]]) && (toClass == classOf[Array[Unit]])) {
      val (source, target) = (from.asInstanceOf[Array[Unit]], to.asInstanceOf[Array[Unit]])
      val srcLast = srcPos + length - 1
      for (i <- Range(0, length)) { target(destPos + i) = source(srcLast - i) }
      target.asInstanceOf[Array[A]]
    } else if (classOf[AnyRef].isAssignableFrom(fromClass) && classOf[AnyRef].isAssignableFrom(toClass)) {
      val (source, target) = (from.asInstanceOf[Array[AnyRef]], to.asInstanceOf[Array[AnyRef]])
      val srcLast = srcPos + length - 1
      for (i <- Range(0, length)) { target(destPos + i) = source(srcLast - i) }
      target.asInstanceOf[Array[A]]
    } else throw new ArrayStoreException("reverseArraycopy does not support type combination (%s, %s)".format(fromClass, toClass))
  }
}
