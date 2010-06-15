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
import org.jfree.chart.plot.{PlotOrientation => JFCPlotOrientation, XYPlot => JFCXYPlot, FastScatterPlot => JFCFastScatter}
import org.jfree.data.{xy => jfcXY}
import org.jfree.data.xy.{XYDataset => JFCXYDataset}
import org.jfree.data.general.{Series => JFCSeries}
import org.jfree.chart.axis.{NumberAxis => JFCNumberAxis}

abstract class Plot extends Proxy {
  def self: JFCChart
  def options: PlotOptions
  
  def draw(): PlotFrame = {
    val frame = PlotFrame(this)
    frame.show()
    frame
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


class XYSeries(title: String = "") extends DataSeries {
  val self = new jfcXY.XYSeries(title)

  def clear(): Unit = self.clear()
  
  def append[TX, TY]
    (points: Seq[(TX, TY)])
    (implicit numX: Numeric[TX], numY: Numeric[TY])
    : Unit = synchronized {
      for (p <- points) self.add(numX.toDouble(p._1), numY.toDouble(p._2))
    }
}

object XYSeries {
  def apply(title: String = ""): XYSeries = new XYSeries(title)

  def apply[TX, TY]
    (title: String, points: (TX, TY)*)
    (implicit numX: Numeric[TX], numY: Numeric[TY])
    : XYSeries =
  {
    val series = new XYSeries(title)
    if (! points.isEmpty) series.append(points)
    series
  }
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


object XYLinePlot {
  def apply(options: XYPlotOptions = XYPlotOptions()): XYLinePlot =
    new XYLinePlot(options)
}


object XYPlot {
  def apply(series: XYSeries, options: XYPlotOptions) : XYPlot = {
    val plot = new XYLinePlot(options)
    plot.addSeries(series)
    plot
  }

  def apply(series: XYSeries) : XYPlot = XYPlot(series, XYPlotOptions())

  def plotPoints[TX,TY]
    (points: Seq[(TX, TY)], options: XYPlotOptions = XYPlotOptions())
    (implicit numX: Numeric[TX], numY: Numeric[TY])
    : XYPlot =
  {
    val series = XYSeries()
    series.append(points)
    XYPlot(series, options)
  }

  def plotValues[A](values: Seq[A], options: XYPlotOptions = XYPlotOptions())(implicit num: Numeric[A]) : XYPlot =
    plotPoints(values.view.zipWithIndex map {_ swap})
}


class ScatterPlot(points: Array[Array[Float]], val options: XYPlotOptions = XYPlotOptions()) extends Plot {
  val jfcPlot = new JFCFastScatter(points,
    new JFCNumberAxis(options.xLabel), new JFCNumberAxis(options.yLabel))

  val self = new JFCChart(options.title,
    JFCChart.DEFAULT_TITLE_FONT, jfcPlot, true)

  def setPoints(points: Array[Array[Float]]) =
    jfcPlot.setData(points)
}


class PlotPanel(plot: Plot) extends Panel {
  override lazy val peer = {
    val jfcPanel = new org.jfree.chart.ChartPanel(plot.self)
    jfcPanel.setMouseWheelEnabled(true)
    jfcPanel
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
