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

import cern.jet.math.Arithmetic.binomial
import daqcore.util._
import scala.math._


class IntArrayIteratorOpt(it: ArrayIterator[Int]) {
  import it._
  
  def opt = this

  def unsupported(reason: String) = throw new UnsupportedOperationException(reason)

  def fsum(): Long = { var acc = 0.toLong; foreach { x => acc += x}; acc }

  def fproduct(): Long = { var acc = 1.toLong; foreach { x => acc *= x}; acc }

  def fmax(): Int = if (!hasNext) unsupported("empty.max()")
    else { var acc = Int.MinValue; foreach { x => if (x > acc) {acc = x} }; acc }

  def fmin(): Int = if (!hasNext) unsupported("empty.min()")
    else { var acc = Int.MaxValue; foreach { x => if (x < acc) {acc = x} }; acc }

  def findMax(): (Int, Int) = if (!hasNext) unsupported("empty.findMax()")
    else { var (i, index, acc) = (-1, -1, Int.MinValue); foreach { x => i += 1; if (x > acc) {index = i; acc = x} }; (index, acc) }

  def findMin(): (Int, Int) = if (!hasNext) unsupported("empty.findMin()")
    else { var (i, index, acc) = (-1, -1, Int.MaxValue); foreach { x => i += 1; if (x < acc) {index = i; acc = x} }; (index, acc) }

  def mean(): Double = if (!hasNext) unsupported("empty.meanVari()")
    else { var len = length; (fsum() / len) }

  def meanVari(): (Double, Double) = {
    if (!hasNext) unsupported("empty.meanVari()")
    var len = length; var (sum1, sum2) = (0.toDouble, 0.toDouble)
    foreach { x => sum1 += x; sum2 += x*x }
    val m = sum1/len
    val v = sum2/len - m*m
    (m, v)
  }

  def meanSigma(): (Double, Double) = if (!hasNext) unsupported("empty.meanSigma()")
    else { val (m, v) = meanVari(); (m, sqrt(v)) }

  def meanError(): (Double, Double) = {
    val len = length
    if (len < 2) unsupported("meanError() undefined for length < 2")
    val (m, v) = meanVari()
    (m, sqrt(v / (len-1)))
  }
}



class LongArrayIteratorOpt(it: ArrayIterator[Long]) {
  import it._
  
  def opt = this

  def unsupported(reason: String) = throw new UnsupportedOperationException(reason)

  def fsum(): Long = { var acc = 0.toLong; foreach { x => acc += x}; acc }

  def fproduct(): Long = { var acc = 1.toLong; foreach { x => acc *= x}; acc }

  def fmax(): Long = if (!hasNext) unsupported("empty.max()")
    else { var acc = Long.MinValue; foreach { x => if (x > acc) {acc = x} }; acc }

  def fmin(): Long = if (!hasNext) unsupported("empty.min()")
    else { var acc = Long.MaxValue; foreach { x => if (x < acc) {acc = x} }; acc }

  def findMax(): (Long, Long) = if (!hasNext) unsupported("empty.findMax()")
    else { var (i, index, acc) = (-1, -1, Long.MinValue); foreach { x => i += 1; if (x > acc) {index = i; acc = x} }; (index, acc) }

  def findMin(): (Long, Long) = if (!hasNext) unsupported("empty.findMin()")
    else { var (i, index, acc) = (-1, -1, Long.MaxValue); foreach { x => i += 1; if (x < acc) {index = i; acc = x} }; (index, acc) }

  def mean(): Double = if (!hasNext) unsupported("empty.meanVari()")
    else { var len = length; (fsum() / len) }

  def meanVari(): (Double, Double) = {
    if (!hasNext) unsupported("empty.meanVari()")
    var len = length; var (sum1, sum2) = (0.toDouble, 0.toDouble)
    foreach { x => sum1 += x; sum2 += x*x }
    val m = sum1/len
    val v = sum2/len - m*m
    (m, v)
  }

  def meanSigma(): (Double, Double) = if (!hasNext) unsupported("empty.meanSigma()")
    else { val (m, v) = meanVari(); (m, sqrt(v)) }

  def meanError(): (Double, Double) = {
    val len = length
    if (len < 2) unsupported("meanError() undefined for length < 2")
    val (m, v) = meanVari()
    (m, sqrt(v / (len-1)))
  }
}



class FloatArrayIteratorOpt(it: ArrayIterator[Float]) {
  import it._
  
  def opt = this

  def unsupported(reason: String) = throw new UnsupportedOperationException(reason)

  def fsum(): Double = { var acc = 0.toDouble; foreach { x => acc += x}; acc }

  def fproduct(): Double = { var acc = 1.toDouble; foreach { x => acc *= x}; acc }

  def fmax(): Float = if (!hasNext) unsupported("empty.max()")
    else { var acc = Float.MinValue; foreach { x => if (x > acc) {acc = x} }; acc }

  def fmin(): Float = if (!hasNext) unsupported("empty.min()")
    else { var acc = Float.MaxValue; foreach { x => if (x < acc) {acc = x} }; acc }

  def findMax(): (Int, Float) = if (!hasNext) unsupported("empty.findMax()")
    else { var (i, index, acc) = (-1, -1, Float.MinValue); foreach { x => i += 1; if (x > acc) {index = i; acc = x} }; (index, acc) }

  def findMin(): (Int, Float) = if (!hasNext) unsupported("empty.findMin()")
    else { var (i, index, acc) = (-1, -1, Float.MaxValue); foreach { x => i += 1; if (x < acc) {index = i; acc = x} }; (index, acc) }

  def mean(): Double = if (!hasNext) unsupported("empty.meanVari()")
    else { var len = length; (fsum() / len) }

  def meanVari(): (Double, Double) = {
    if (!hasNext) unsupported("empty.meanVari()")
    var len = length; var (sum1, sum2) = (0.toDouble, 0.toDouble)
    foreach { x => sum1 += x; sum2 += x*x }
    val m = sum1/len
    val v = sum2/len - m*m
    (m, v)
  }

  def meanSigma(): (Double, Double) = if (!hasNext) unsupported("empty.meanSigma()")
    else { val (m, v) = meanVari(); (m, sqrt(v)) }

  def meanError(): (Double, Double) = {
    val len = length
    if (len < 2) unsupported("meanError() undefined for length < 2")
    val (m, v) = meanVari()
    (m, sqrt(v / (len-1)))
  }
}



class DoubleArrayIteratorOpt(it: ArrayIterator[Double]) {
  import it._
  
  def opt = this

  def unsupported(reason: String) = throw new UnsupportedOperationException(reason)

  def fsum(): Double = { var acc = 0.toDouble; foreach { x => acc += x}; acc }

  def fproduct(): Double = { var acc = 1.toDouble; foreach { x => acc *= x}; acc }

  def fmax(): Double = if (!hasNext) unsupported("empty.max()")
    else { var acc = Double.MinValue; foreach { x => if (x > acc) {acc = x} }; acc }

  def fmin(): Double = if (!hasNext) unsupported("empty.min()")
    else { var acc = Double.MaxValue; foreach { x => if (x < acc) {acc = x} }; acc }

  def findMax(): (Int, Double) = if (!hasNext) unsupported("empty.findMax()")
    else { var (i, index, acc) = (-1, -1, Double.MinValue); foreach { x => i += 1; if (x > acc) {index = i; acc = x} }; (index, acc) }

  def findMin(): (Int, Double) = if (!hasNext) unsupported("empty.findMin()")
    else { var (i, index, acc) = (-1, -1, Double.MaxValue); foreach { x => i += 1; if (x < acc) {index = i; acc = x} }; (index, acc) }

  def mean(): Double = if (!hasNext) unsupported("empty.meanVari()")
    else { var len = length; (fsum() / len) }

  def meanVari(): (Double, Double) = {
    if (!hasNext) unsupported("empty.meanVari()")
    var len = length; var (sum1, sum2) = (0.toDouble, 0.toDouble)
    foreach { x => sum1 += x; sum2 += x*x }
    val m = sum1/len
    val v = sum2/len - m*m
    (m, v)
  }

  def meanSigma(): (Double, Double) = if (!hasNext) unsupported("empty.meanSigma()")
    else { val (m, v) = meanVari(); (m, sqrt(v)) }

  def meanError(): (Double, Double) = {
    val len = length
    if (len < 2) unsupported("meanError() undefined for length < 2")
    val (m, v) = meanVari()
    (m, sqrt(v / (len-1)))
  }
}
