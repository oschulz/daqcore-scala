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

import scala.swing._

import org.jfree.chart.{ChartPanel => JFCPanel, JFreeChart => JFCChart, ChartFactory => JFCChartFactory }
import org.jfree.chart.plot.{PlotOrientation => JFCPlotOrientation, XYPlot => JFCXYPlot}
import org.jfree.data.{xy => jfcXY}
import org.jfree.data.xy.{XYDataset => JFCXYDataset}
import org.jfree.data.general.{Series => JFCSeries}


abstract class Plot extends Proxy {
  def self: JFCChart
  def options: PlotOptions
  
  def draw(): Unit = {
    val frame = PlotFrame(this)
    frame.show()
  }
}



abstract class PlotOptions {}



case class XYPlotOptions (
    title: String = "",
    xLabel: String = "",
    yLabel: String = "",
    withLegend: Boolean = true
) extends PlotOptions




abstract class DataSeries extends Proxy {
  override def self: JFCSeries
}


case class XYSeries(title: String = "") extends DataSeries {
  val self = new jfcXY.XYSeries(title)

  def addPoints(points: TraversableOnce[(Double, Double)]) : Unit =
    for (p <- points) self.add(p._1, p._2)
}



abstract class XYPlot extends Plot {
  override def options: XYPlotOptions
}



class XYLinePlot(val options: XYPlotOptions) extends XYPlot {
  protected def jfcLineChart(dataset: JFCXYDataset): JFCChart = {
    JFCChartFactory.createXYLineChart(
      options.title, options.xLabel, options.yLabel,
      dataset,
      JFCPlotOrientation.VERTICAL,
      options.withLegend,
      true, // tooltips
      false // urls
    )
  }

  val jfcDataset = new jfcXY.XYSeriesCollection()

  val self = jfcLineChart(jfcDataset)

  def addSeries(series: XYSeries) : Unit = jfcDataset.addSeries(series.self)
}


object XYPlot {
  def apply(series: XYSeries, options: XYPlotOptions) : XYPlot = {
    val plot = new XYLinePlot(options)
    plot.addSeries(series)
    plot
  }

  def apply(series: XYSeries) : XYPlot = XYPlot(series, XYPlotOptions())

  def apply(points: TraversableOnce[(Double, Double)], options: XYPlotOptions = XYPlotOptions()) : XYPlot = {
    val series = XYSeries()
    series.addPoints(points)
    XYPlot(series, options)
  }
}



class PlotPanel(plot: Plot) extends Panel {
  override lazy val peer = {
    new org.jfree.chart.ChartPanel(plot.self)
  }
}

object PlotPanel {
  def apply(plot: Plot) = new PlotPanel(plot)
}



case class PlotFrame(plot: Plot, frameTitle: String = "Plot") extends Frame {
  title = frameTitle
  contents = PlotPanel(plot)
  
  def show() = Swing.onEDT {
    pack()
    visible = true
  }
  
  show()
}
