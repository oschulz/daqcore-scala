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

import daqcore.util._
import daqcore.actors._
import daqcore.profiles._
import daqcore.monads._
import daqcore.data._

import org.jfree.chart.{event => jfcEvent}


class Scope(source: EventSource) extends Server with Closeable {
  object DrawingFinished
  case class Draw(ev: raw.Event)

  def maxRange(a: Range, b: Range) = math.min(a.head, b.head) to math.max(a.last, b.last)

  val plot = XYLinePlot()

  val channels = 1 to 9
  val chSeries = Map ( (
    for (ch <- channels) yield { ch -> XYSeries("Ch%s".format(ch)) }
  ) : _*)
  for (ch <- channels) plot.addSeries(chSeries(ch))

  object drawProgressListener extends jfcEvent.ChartProgressListener {
    def enableOnce() : Unit = plot.self.addProgressListener(this)
    def chartProgress(event: jfcEvent.ChartProgressEvent) = {
      if (event.getType == jfcEvent.ChartProgressEvent.DRAWING_FINISHED ) {
        plot.self.removeProgressListener(this)
        srv ! DrawingFinished
      }
    }
  }

  object frame extends Frame {
    title = "Scope"
    
    contents = PlotPanel(plot)

    def draw(ev: raw.Event) = Swing.onEDT {
      val xRanges = for ((ch, trans) <- ev.trans) yield
        ch -> Range(0 - trans.trigPos, trans.samples.size - trans.trigPos)
      
      val yRanges = for ((ch, trans) <- ev.trans) yield
        ch -> (trans.samples.min to trans.samples.max)
      
      if (plot.self.getXYPlot.getDomainAxis.isAutoRange) {
        val xRange = xRanges.values.reduceLeft(maxRange)
        plot.self.getXYPlot.getDomainAxis.setLowerBound(xRange.head)
        plot.self.getXYPlot.getDomainAxis.setUpperBound(xRange.last)
      }
      if (plot.self.getXYPlot.getRangeAxis.isAutoRange) {
        val yRange = maxRange(0 to 4095, yRanges.values.reduceLeft(maxRange))
        plot.self.getXYPlot.getRangeAxis.setLowerBound(yRange.head)
        plot.self.getXYPlot.getRangeAxis.setUpperBound(yRange.last)
      }
      
      for ((ch, trans) <- ev.trans) chSeries(ch).clear()

      for ((ch, trans) <- ev.trans)
        chSeries(ch) append ( xRanges(ch).view zip trans.samples )
    }
    
    def show() = Swing.onEDT {
      pack()
      visible = true
    }
  }
  
  override protected def init() = {
    super.init()
    frame.show()
    srv ! DrawingFinished
  }

  protected def serve = {
    case DrawingFinished => {
      trace("Drawing finished")
      source addHandlerFunc { case ev:raw.Event => srv ! Draw(ev); false }
    }
    case Draw(ev) => {
      frame.draw(ev)
      drawProgressListener.enableOnce()
    }
    case Closeable.Close => {
      debug("Closing")
      frame.close()
    }
  }
}
