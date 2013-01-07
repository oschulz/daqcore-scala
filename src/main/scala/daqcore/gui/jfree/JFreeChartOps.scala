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
package jfree

import scala.swing._

import org.jfree.chart._
import org.jfree.chart.plot.{PlotOrientation, XYPlot, FastScatterPlot}
import org.jfree.data.general.Series
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.event._

import daqcore.util._


class JFreeChartOps(chart: JFreeChart) {

  class ChartProgressHandler(handlerFunc: PartialFunction[ChartProgressEvent, Boolean])
    extends ChartProgressListener with Logging
  {
    def chartProgress(event: ChartProgressEvent) = {
      if (handlerFunc isDefinedAt event) {
        var again = false
        try { again = handlerFunc(event) }
        catch { case e: Throwable => log.error(e.getMessage) }
        finally { if (!again) chart.removeProgressListener(this) }
      }
    }
  }
  
  def onProgress(handlerFunc: PartialFunction[ChartProgressEvent, Boolean]): Unit = {
    val handler = new ChartProgressHandler(handlerFunc)
    synchedOnEDT { chart.addProgressListener(handler) }
  }

  def draw(title: String = ""): SwingChartFrame = {
    val frame = SwingChartFrame(chart, title)
    frame.show()
    frame
  }
  
  def xyPlot = chart.getPlot.asInstanceOf[XYPlot]
  def xyDataset = xyPlot.getDataset
  def xyDatasets = xyPlot.getDataset.asInstanceOf[XYSeriesCollection]
}


abstract class JFreeChartCompanion {

  object XY {
    case class Options (
      title: String = "",
      xLabel: String = "",
      yLabel: String = "",
      withLegend: Boolean = true
    )
  
    def apply(dataset: XYDataset = new XYSeriesCollection, options: Options = Options()): JFreeChart = {
       val chart = ChartFactory.createXYLineChart (
          options.title, options.xLabel, options.yLabel,
          dataset,
          PlotOrientation.VERTICAL,
          options.withLegend,
          true, // tooltips
          false // urls
        )
      chart
    }
    
    def ofPoints[TX,TY]
      (points: Seq[(TX, TY)], options: Options = Options())
      (implicit numX: Numeric[TX], numY: Numeric[TY])
      : JFreeChart =
    {
      val chart = apply(options = options)
      val series = XYSeries()
      series ++= points
      chart.xyDatasets.addSeries(series)
      chart
    }

    def ofValues[A](values: Seq[A], options: Options = Options())(implicit num: Numeric[A]): JFreeChart = {
      ofPoints(values.view.zipWithIndex map {_.swap}, options)
    }
  }

}
