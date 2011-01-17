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


package daqcore.gui


package object jfree {
  import org.jfree.chart._
  import org.jfree.data.{xy => jfreeXY}

  type Chart = JFreeChart
  object Chart extends JFreeChartCompanion
  implicit def jFreeChartOps(chart: JFreeChart) = new JFreeChartOps(chart)

  type XYDataset = jfreeXY.XYDataset
  object XYDataset extends XYDatasetCompanion
  implicit def xyDatasetOps(dataset: XYDataset) = new XYDatasetOps(dataset)

  type XYSeries = jfreeXY.XYSeries
  type XYSeriesCollection = jfreeXY.XYSeriesCollection
  object XYSeries extends XYSeriesCompanion
  implicit def xySeriesOps(series: XYSeries) = new XYSeriesOps(series)
}
