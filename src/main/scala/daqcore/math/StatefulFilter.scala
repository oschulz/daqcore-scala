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


package daqcore.math

import daqcore.util._


trait StatefulFilter[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) A, @specialized(scala.Int, scala.Long, scala.Float, scala.Double) B] extends Mutable with Function[A,B]


// y(n) = 1/a(0) * ( (b(0)*x(n-0) + b(1) * x(n-1) + ...
//                  - a(1) * y(n-1) - a(2) * y(n-2) - ... )

abstract class IIRFilter extends StatefulFilter[Double, Double] {
  def a: IndexedSeq[Double]
  def b: IndexedSeq[Double]

  var initialized = false
  var x: RingBuffer[Double] = null
  var y: RingBuffer[Double] = null
  
  def apply(in: Double) = {
    if (!initialized) {
      x = RingBuffer(b.size, in)
      y = RingBuffer(a.size - 1, in)
      initialized = true
    } else x.pushFront(in)
    
    var out = 0.;
    for (i <- 0 to b.size - 1) out += b(i)*x(i)
    for (i <- 1 to a.size - 1) out -= a(i)*y(i-1)
    out = out / a(0)
    
    y.pushFront(out)
    out
  }
}


object IIRFilter {
  def apply(ca: IndexedSeq[Double], cb:IndexedSeq[Double]) = new IIRFilter {
    def a = ca
    def b = cb
  }
}


case class RCFilter(c: Double) extends IIRFilter {
  val alpha = 1. / (1. + c)
  val a = Vector(1., alpha-1)
  val b = Vector(alpha)
}


case class DifferenceFilter[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) A](implicit num: scala.math.Numeric[A]) extends StatefulFilter[A, A] {
  var last: A = num.zero
  
  @specialized def apply(x: A): A = {
    val next = num.minus(x, last)
    last = x
    next
  }
}


case class IntegrateFilter[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) A](implicit num: scala.math.Numeric[A]) extends StatefulFilter[A, A] {
  var last: A = num.zero
  
  @specialized def apply(x: A): A = {
    val next = num.plus(x, last)
    last = next
    next
  }
}
