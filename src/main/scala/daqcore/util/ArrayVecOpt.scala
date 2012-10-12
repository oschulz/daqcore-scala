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

import daqcore.util._


class IntArrayVecOpt(seq: ArrayVec[Int]) {
  import seq._
  
  def opt = this

  def foldWith(kernel: ArrayVec[Int]): ArrayVec[Int] = {
    require(seq.length >= kernel.length)
    val target = Array.ofDim[Int](seq.length - kernel.length + 1)
    for (i <- Range(0, target.length))
      { var acc = 0.toInt; for { j <- Range(0, kernel.length) } acc += seq(i + j) * kernel(j); target(i) = acc }
    ArrayVec.wrap(target)
  }

  def sprod(that: ArrayVec[Int]): Long = {
    require(seq.size == that.size)
    var acc = 0.toLong; for { i <- Range(0, seq.length) } acc += seq(i) * that(i); acc
  }

  def fsum = iterator.opt.fsum()
  def fproduct = iterator.opt.fproduct()
  def fmax = iterator.opt.fmax()
  def fmin = iterator.opt.fmin()
  def findMax = iterator.opt.findMax()
  def findMin = iterator.opt.findMin()
  def mean = iterator.opt.mean()
  def meanVari = iterator.opt.meanVari()
  def meanSigma = iterator.opt.meanSigma()
  def meanError = iterator.opt.meanError()

  def toInt: ArrayVec[Int] = seq
  def toLong: ArrayVec[Long] = { val target = Array.ofDim[Long](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toLong }; ArrayVec.wrap(target) }
  def toFloat: ArrayVec[Float] = { val target = Array.ofDim[Float](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toFloat }; ArrayVec.wrap(target) }
  def toDouble: ArrayVec[Double] = { val target = Array.ofDim[Double](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toDouble }; ArrayVec.wrap(target) }

  def map[B: ClassTag] (op: Int => B): ArrayVec[B] = {
    val mfB = classTag[B]
    
    if (mfB == classTag[Unit]) {
      val f = op.asInstanceOf[Int => Unit]
      val target = Array.ofDim[Unit](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Int]) {
      val f = op.asInstanceOf[Int => Int]
      val target = Array.ofDim[Int](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Long]) {
      val f = op.asInstanceOf[Int => Long]
      val target = Array.ofDim[Long](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Float]) {
      val f = op.asInstanceOf[Int => Float]
      val target = Array.ofDim[Float](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Double]) {
      val f = op.asInstanceOf[Int => Double]
      val target = Array.ofDim[Double](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else throw new UnsupportedOperationException("...ArrayVecOpt.map does not support target type " + mfB)
  }
}


class LongArrayVecOpt(seq: ArrayVec[Long]) {
  import seq._
  
  def opt = this

  def foldWith(kernel: ArrayVec[Long]): ArrayVec[Long] = {
    require(seq.length >= kernel.length)
    val target = Array.ofDim[Long](seq.length - kernel.length + 1)
    for (i <- Range(0, target.length))
      { var acc = 0.toLong; for { j <- Range(0, kernel.length) } acc += seq(i + j) * kernel(j); target(i) = acc }
    ArrayVec.wrap(target)
  }

  def sprod(that: ArrayVec[Long]): Long = {
    require(seq.size == that.size)
    var acc = 0.toLong; for { i <- Range(0, seq.length) } acc += seq(i) * that(i); acc
  }

  def fsum = iterator.opt.fsum()
  def fproduct = iterator.opt.fproduct()
  def fmax = iterator.opt.fmax()
  def fmin = iterator.opt.fmin()
  def findMax = iterator.opt.findMax()
  def findMin = iterator.opt.findMin()
  def mean = iterator.opt.mean()
  def meanVari = iterator.opt.meanVari()
  def meanSigma = iterator.opt.meanSigma()
  def meanError = iterator.opt.meanError()

  def toInt: ArrayVec[Int] = { val target = Array.ofDim[Int](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toInt }; ArrayVec.wrap(target) }
  def toLong: ArrayVec[Long] = seq
  def toFloat: ArrayVec[Float] = { val target = Array.ofDim[Float](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toFloat }; ArrayVec.wrap(target) }
  def toDouble: ArrayVec[Double] = { val target = Array.ofDim[Double](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toDouble }; ArrayVec.wrap(target) }

  def map[B: ClassTag] (op: Long => B): ArrayVec[B] = {
    val mfB = classTag[B]
    
    if (mfB == classTag[Unit]) {
      val f = op.asInstanceOf[Long => Unit]
      val target = Array.ofDim[Unit](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Int]) {
      val f = op.asInstanceOf[Long => Int]
      val target = Array.ofDim[Int](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Long]) {
      val f = op.asInstanceOf[Long => Long]
      val target = Array.ofDim[Long](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Float]) {
      val f = op.asInstanceOf[Long => Float]
      val target = Array.ofDim[Float](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Double]) {
      val f = op.asInstanceOf[Long => Double]
      val target = Array.ofDim[Double](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else throw new UnsupportedOperationException("...ArrayVecOpt.map does not support target type " + mfB)
  }
}


class FloatArrayVecOpt(seq: ArrayVec[Float]) {
  import seq._
  
  def opt = this

  def foldWith(kernel: ArrayVec[Float]): ArrayVec[Float] = {
    require(length >= kernel.length)
    val target = Array.ofDim[Float](length - kernel.length + 1)
    for (i <- Range(0, target.length))
      { var acc = 0.toFloat; for { j <- Range(0, kernel.length) } acc += seq(i + j) * kernel(j); target(i) = acc }
    ArrayVec.wrap(target)
  }

  def sprod(that: ArrayVec[Float]): Double = {
    require(seq.length == that.length)
    var acc = 0.toDouble; for { i <- Range(0, seq.length) } acc += seq(i) * that(i); acc
  }

  def fsum = iterator.opt.fsum()
  def fproduct = iterator.opt.fproduct()
  def fmax = iterator.opt.fmax()
  def fmin = iterator.opt.fmin()
  def findMax = iterator.opt.findMax()
  def findMin = iterator.opt.findMin()
  def mean = iterator.opt.mean()
  def meanVari = iterator.opt.meanVari()
  def meanSigma = iterator.opt.meanSigma()
  def meanError = iterator.opt.meanError()

  def toInt: ArrayVec[Int] = { val target = Array.ofDim[Int](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toInt }; ArrayVec.wrap(target) }
  def toLong: ArrayVec[Long] = { val target = Array.ofDim[Long](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toLong }; ArrayVec.wrap(target) }
  def toFloat: ArrayVec[Float] = seq
  def toDouble: ArrayVec[Double] = { val target = Array.ofDim[Double](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toDouble }; ArrayVec.wrap(target) }

  def map[B: ClassTag] (op: Float => B): ArrayVec[B] = {
    val mfB = classTag[B]
    
    if (mfB == classTag[Unit]) {
      val f = op.asInstanceOf[Float => Unit]
      val target = Array.ofDim[Unit](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Int]) {
      val f = op.asInstanceOf[Float => Int]
      val target = Array.ofDim[Int](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Long]) {
      val f = op.asInstanceOf[Float => Long]
      val target = Array.ofDim[Long](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Float]) {
      val f = op.asInstanceOf[Float => Float]
      val target = Array.ofDim[Float](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Double]) {
      val f = op.asInstanceOf[Float => Double]
      val target = Array.ofDim[Double](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else throw new UnsupportedOperationException("...ArrayVecOpt.map does not support target type " + mfB)
  }
}


class DoubleArrayVecOpt(seq: ArrayVec[Double]) {
  import seq._
  
  def opt = this

  def foldWith(kernel: ArrayVec[Double]): ArrayVec[Double] = {
    require(seq.length >= kernel.length)
    val target = Array.ofDim[Double](seq.length - kernel.length + 1)
    for (i <- Range(0, target.length))
      { var acc = 0.toDouble; for { j <- Range(0, kernel.length) } acc += seq(i + j) * kernel(j); target(i) = acc }
    ArrayVec.wrap(target)
  }

  def sprod(that: ArrayVec[Double]): Double = {
    require(seq.size == that.size)
    var acc = 0.toDouble; for { i <- Range(0, seq.length) } acc += seq(i) * that(i); acc
  }

  def fsum = iterator.opt.fsum()
  def fproduct = iterator.opt.fproduct()
  def fmax = iterator.opt.fmax()
  def fmin = iterator.opt.fmin()
  def findMax = iterator.opt.findMax()
  def findMin = iterator.opt.findMin()
  def mean = iterator.opt.mean()
  def meanVari = iterator.opt.meanVari()
  def meanSigma = iterator.opt.meanSigma()
  def meanError = iterator.opt.meanError()
  
  def toInt: ArrayVec[Int] = { val target = Array.ofDim[Int](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toInt }; ArrayVec.wrap(target) }
  def toLong: ArrayVec[Long] = { val target = Array.ofDim[Long](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toLong }; ArrayVec.wrap(target) }
  def toFloat: ArrayVec[Float] = { val target = Array.ofDim[Float](seq.length); for (i <- Range(0,seq.length)) { target(i) = seq(i).toFloat }; ArrayVec.wrap(target) }
  def toDouble: ArrayVec[Double] = seq

  def map[B: ClassTag] (op: Double => B): ArrayVec[B] = {
    val mfB = classTag[B]
    
    if (mfB == classTag[Unit]) {
      val f = op.asInstanceOf[Double => Unit]
      val target = Array.ofDim[Unit](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Int]) {
      val f = op.asInstanceOf[Double => Int]
      val target = Array.ofDim[Int](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Long]) {
      val f = op.asInstanceOf[Double => Long]
      val target = Array.ofDim[Long](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Float]) {
      val f = op.asInstanceOf[Double => Float]
      val target = Array.ofDim[Float](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else if (mfB == classTag[Double]) {
      val f = op.asInstanceOf[Double => Double]
      val target = Array.ofDim[Double](length)
      for (index <- Range(0, length)) target(index) = f(seq(index))
      ArrayVec.wrap(target).asInstanceOf[ArrayVec[B]]
    } else throw new UnsupportedOperationException("...ArrayVecOpt.map does not support target type " + mfB)
  }
}
