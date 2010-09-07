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

import scala.math._

package object math {


def hammingWindow(N:Int): PartialFunction[Int, Double] = _ match {
  case i if ((i >= 0) && (i < N)) => {
    0.54 - 0.46 * cos(2*Pi*i / (N-1))
  }
}

def blackmanWindow(N:Int): PartialFunction[Int, Double] = _ match {
  case i if ((i >= 0) && (i < N)) => {
    def a = 0.16
    
    +((1.-a)/2.) +
    -(1./2.) * cos(2*Pi*i / (N-1)) +
    +(a/2.) * cos(4*Pi*i / (N-1))
  }
}

def blackmanHarrisWindow(N:Int): PartialFunction[Int, Double] = _ match {
  case i if ((i >= 0) && (i < N)) => {
    +0.35875 +
    -0.48829 * cos(2*Pi*i / (N-1)) +
    +0.14128 * cos(4*Pi*i / (N-1)) +
    -0.01168 * cos(6*Pi*i / (N-1))
  }
}

def kaiserWindow(alpha:Int)(N:Int): PartialFunction[Int, Double] = _ match {
  case i if ((i >= 0) && (i < N)) => {
    import cern.jet.math.Bessel.i0
    
    i0(
      Pi * alpha * sqrt(1- pow( (2.*i/(N-1)) - 1, 2) )
    ) / i0 (Pi * alpha)
  }
}

def turkeyWindow(N:Int): PartialFunction[Int, Double] = _ match {
  case i if ((i >= 0) && (i < N)) => {
    val a = 0.5
    if ((i > a * N / 2) && (i <= (N-1) - a * N / 2 )) 1.0
    else 0.5 * (1 + cos (Pi * (i - a*N/2) / ((1-a)*N/2)))
  }
}


def harmN(N:Int)(n:Double)(phase: Double)(i:Int) = sin(n * 2*Pi*i / (N-1) + phase)

}
