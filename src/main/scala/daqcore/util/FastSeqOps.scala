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


case class FastSeqOps[T: ClassManifest](seq: Seq[T]) {
  val mfT = classManifest[T]
  lazy val array = seq.toArray


  def take(n: Int): IndexedSeq[T] = {
    if (n >= seq.size) seq.toIISeq
    else if (n <= 0) IISeq[T]()
    else if (mfT == classManifest[Int]) {
      val inArray = array.asInstanceOf[Array[Int]]
      val outArray = Array.ofDim[Int](n)
      for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i)
      outArray.toSeq.asInstanceOf[IndexedSeq[T]]
    } else if (mfT == classManifest[Boolean]) {
      val inArray = array.asInstanceOf[Array[Boolean]]
      val outArray = Array.ofDim[Boolean](n)
      for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i)
      outArray.toSeq.asInstanceOf[IndexedSeq[T]]
    } else if (mfT == classManifest[Float]) {
      val inArray = array.asInstanceOf[Array[Float]]
      val outArray = Array.ofDim[Float](n)
      for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i)
      outArray.toSeq.asInstanceOf[IndexedSeq[T]]
    } else if (mfT == classManifest[Double]) {
      val inArray = array.asInstanceOf[Array[Double]]
      val outArray = Array.ofDim[Double](n)
      for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i)
      outArray.toSeq.asInstanceOf[IndexedSeq[T]]
    } else throw new UnsupportedOperationException("FastOps.take does not support (%s)".format(mfT))
  }

  
  def drop(n: Int): IndexedSeq[T] = {
    if (n >= seq.size) IISeq[T]()
    else if (n <= 0) seq.toIISeq
    else if (mfT == classManifest[Int]) {
      val inArray = array.asInstanceOf[Array[Int]]
      val outArray = Array.ofDim[Int](inArray.size - n)
      for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i + n)
      outArray.toSeq.asInstanceOf[IndexedSeq[T]]
    } else if (mfT == classManifest[Boolean]) {
      val inArray = array.asInstanceOf[Array[Boolean]]
      val outArray = Array.ofDim[Boolean](inArray.size - n)
      for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i + n)
      outArray.toSeq.asInstanceOf[IndexedSeq[T]]
    } else if (mfT == classManifest[Float]) {
      val inArray = array.asInstanceOf[Array[Float]]
      val outArray = Array.ofDim[Float](inArray.size - n)
      for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i + n)
      outArray.toSeq.asInstanceOf[IndexedSeq[T]]
    } else if (mfT == classManifest[Double]) {
      val inArray = array.asInstanceOf[Array[Double]]
      val outArray = Array.ofDim[Double](inArray.size - n)
      for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i + n)
      outArray.toSeq.asInstanceOf[IndexedSeq[T]]
    } else throw new UnsupportedOperationException("FastOps.drop does not support (%s)".format(mfT))
  }
  
  
  def splitAt(n: Int): (IndexedSeq[T], IndexedSeq[T]) = (take(n), drop(n))


  def grouped(n:Int) : Iterator[IndexedSeq[T]] = {
    require(n > 0)
    val nGroups = (seq.size + n - 1) / n
    if (nGroups == 1) IISeq(seq.toIISeq).iterator
    else if  (mfT == classManifest[Int]) {
      val inArray = array.asInstanceOf[Array[Int]]
      val outArrays = Array.ofDim[IndexedSeq[T]](nGroups)
      for (group <- 0 to nGroups - 1) {
        val outArray = Array.ofDim[Int](n min (inArray.size - n * group))
        for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i + n * group)
        outArrays(group) = outArray.toSeq.asInstanceOf[IndexedSeq[T]]
      }
      outArrays.toSeq.iterator
    } else if  (mfT == classManifest[Boolean]) {
      val inArray = array.asInstanceOf[Array[Boolean]]
      val outArrays = Array.ofDim[IndexedSeq[T]](nGroups)
      for (group <- 0 to nGroups - 1) {
        val outArray = Array.ofDim[Boolean](n min (inArray.size - n * group))
        for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i + n * group)
        outArrays(group) = outArray.toSeq.asInstanceOf[IndexedSeq[T]]
      }
      outArrays.toSeq.iterator
    } else if  (mfT == classManifest[Float]) {
      val inArray = array.asInstanceOf[Array[Float]]
      val outArrays = Array.ofDim[IndexedSeq[T]](nGroups)
      for (group <- 0 to nGroups - 1) {
        val outArray = Array.ofDim[Float](n min (inArray.size - n * group))
        for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i + n * group)
        outArrays(group) = outArray.toSeq.asInstanceOf[IndexedSeq[T]]
      }
      outArrays.toSeq.iterator
    } else if  (mfT == classManifest[Double]) {
      val inArray = array.asInstanceOf[Array[Double]]
      val outArrays = Array.ofDim[IndexedSeq[T]](nGroups)
      for (group <- 0 to nGroups - 1) {
        val outArray = Array.ofDim[Double](n min (inArray.size - n * group))
        for (i <- 0 to outArray.size - 1) outArray(i) = inArray(i + n * group)
        outArrays(group) = outArray.toSeq.asInstanceOf[IndexedSeq[T]]
      }
      outArrays.toSeq.iterator
    } else throw new UnsupportedOperationException("FastOps.group does not support (%s)".format(mfT))
  }
  

  def map[U: ClassManifest](f: Function1[T, U]): IndexedSeq[U] = {
    val mfU = classManifest[U]
    
    if ((mfT == classManifest[Int]) && (mfU == classManifest[Int])) {
      val inArray = array.asInstanceOf[Array[Int]]
      val outArray = Array.ofDim[Int](inArray.size)
      for (i <- 0 to inArray.size-1) outArray(i) = f.asInstanceOf[Function[Int, Int]](inArray(i))
      outArray.toSeq.asInstanceOf[IndexedSeq[U]]
    } else if ((mfT == classManifest[Int]) && (mfU == classManifest[Boolean])) {
      val inArray = array.asInstanceOf[Array[Int]]
      val outArray = Array.ofDim[Boolean](inArray.size)
      for (i <- 0 to inArray.size-1) outArray(i) = f.asInstanceOf[Function[Int, Boolean]](inArray(i))
      outArray.toSeq.asInstanceOf[IndexedSeq[U]]
    } else if ((mfT == classManifest[Int]) && (mfU == classManifest[Float])) {
      val inArray = array.asInstanceOf[Array[Int]]
      val outArray = Array.ofDim[Float](inArray.size)
      for (i <- 0 to inArray.size-1) outArray(i) = f.asInstanceOf[Function[Int, Float]](inArray(i))
      outArray.toSeq.asInstanceOf[IndexedSeq[U]]
    } else if ((mfT == classManifest[Int]) && (mfU == classManifest[Double])) {
      val inArray = array.asInstanceOf[Array[Int]]
      val outArray = Array.ofDim[Double](inArray.size)
      for (i <- 0 to inArray.size-1) outArray(i) = f.asInstanceOf[Function[Int, Double]](inArray(i))
      outArray.toSeq.asInstanceOf[IndexedSeq[U]]
    } else if ((mfT == classManifest[Float]) && (mfU == classManifest[Float])) {
      val inArray = array.asInstanceOf[Array[Float]]
      val outArray = Array.ofDim[Float](inArray.size)
      for (i <- 0 to inArray.size-1) outArray(i) = f.asInstanceOf[Function[Float, Float]](inArray(i))
      outArray.toSeq.asInstanceOf[IndexedSeq[U]]
    } else if ((mfT == classManifest[Float]) && (mfU == classManifest[Int])) {
      val inArray = array.asInstanceOf[Array[Float]]
      val outArray = Array.ofDim[Int](inArray.size)
      for (i <- 0 to inArray.size-1) outArray(i) = f.asInstanceOf[Function[Float, Int]](inArray(i))
      outArray.toSeq.asInstanceOf[IndexedSeq[U]]
    } else if ((mfT == classManifest[Double]) && (mfU == classManifest[Double])) {
      val inArray = array.asInstanceOf[Array[Double]]
      val outArray = Array.ofDim[Double](inArray.size)
      for (i <- 0 to inArray.size-1) outArray(i) = f.asInstanceOf[Function[Double, Double]](inArray(i))
      outArray.toSeq.asInstanceOf[IndexedSeq[U]]
    } else if ((mfT == classManifest[Double]) && (mfU == classManifest[Double])) {
      val inArray = array.asInstanceOf[Array[Int]]
      val outArray = Array.ofDim[Double](inArray.size)
      for (i <- 0 to inArray.size-1) outArray(i) = f.asInstanceOf[Function[Double, Int]](inArray(i))
      outArray.toSeq.asInstanceOf[IndexedSeq[U]]
    } else throw new UnsupportedOperationException("FastOps.map does not support (%s, %s)".format(mfT, mfU))
  }
}
