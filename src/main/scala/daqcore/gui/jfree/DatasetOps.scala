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


package daqcore.gui.jfree

import scala.swing._

import daqcore.util._


class XYDatasetOps(dataset: XYDataset) {
}

abstract class XYDatasetCompanion {
}


class XYSeriesOps(series: XYSeries) {
  def ++=[TX, TY]
    (points: Seq[(TX, TY)])
    (implicit numX: Numeric[TX], numY: Numeric[TY])
    : XYSeries = {
      Swing.onEDT { for (p <- points) series.add(numX.toDouble(p._1), numY.toDouble(p._2)) }
      series
    }
}


abstract class XYSeriesCompanion {
  def apply(title: String = ""): XYSeries = new XYSeries(title)

  def apply[TX, TY]
    (title: String, points: (TX, TY)*)
    (implicit numX: Numeric[TX], numY: Numeric[TY])
    : XYSeries =
  {
    val series = apply(title)
    if (! points.isEmpty) series ++= points
    series
  }
}
