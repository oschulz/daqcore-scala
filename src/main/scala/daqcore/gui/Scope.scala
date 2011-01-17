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

import akka.actor._, akka.actor.Actor._
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}

import daqcore.util._
import daqcore.actors._
import daqcore.servers._
import daqcore.profiles._
import daqcore.monads._
import daqcore.data._

import org.jfree.chart.event.ChartProgressEvent

import daqcore.gui.jfree._


class Scope(source: EventSource) extends CloseableServer {
  override def profiles = super.profiles.+[Closeable]

  object DrawingFinished
  case class Draw(ev: raw.Event)

  def maxRange(a: Range, b: Range) = math.min(a.head, b.head) to math.max(a.last, b.last)

  val chart = Chart.XY()

  val channels = 1 to 9
  val chSeries = ( for (ch <- channels) yield { ch -> jfree.XYSeries("Ch%s".format(ch)) } ).toMap
  for (ch <- channels) chart.xyDatasets.addSeries(chSeries(ch))

  object frame extends Frame {
    title = "Scope"
    
    contents = SwingChartPanel(chart)

    def draw(ev: raw.Event) = synchedOnEDT {
      val xRanges = for ((ch, trans) <- ev.trans) yield
        ch -> Range(0 - trans.trigPos, trans.samples.size - trans.trigPos)
      
      val yRanges = for ((ch, trans) <- ev.trans) yield
        ch -> (trans.samples.min to trans.samples.max)
      
      if (chart.getXYPlot.getDomainAxis.isAutoRange) {
        val xRange = xRanges.values.reduceLeft(maxRange)
        chart.getXYPlot.getDomainAxis.setLowerBound(xRange.head)
        chart.getXYPlot.getDomainAxis.setUpperBound(xRange.last)
      }
      if (chart.getXYPlot.getRangeAxis.isAutoRange) {
        val yRange = maxRange(0 to 4095, yRanges.values.reduceLeft(maxRange))
        chart.getXYPlot.getRangeAxis.setLowerBound(yRange.head)
        chart.getXYPlot.getRangeAxis.setUpperBound(yRange.last)
      }
      
      for ((ch, trans) <- ev.trans) chSeries(ch).clear()

      for ((ch, trans) <- ev.trans)
        chSeries(ch) ++= xRanges(ch).view zip trans.samples
    }
    
    def show() = synchedOnEDT {
      pack()
      visible = true
    }
  }
  
  override def init() = {
    super.init()
    clientLinkTo(source.srv)
    frame.show()
    srv ! DrawingFinished
  }

  override def serve = super.serve orElse {
    case DrawingFinished => {
      trace("Drawing finished")
      source addHandlerFunc { case ev:raw.Event => srv ! Draw(ev); false }
    }

    case Draw(ev) => {
      chart onProgress {
        case ev if ev.getType == ChartProgressEvent.DRAWING_FINISHED =>
          { srv ! DrawingFinished; false }
      }
      frame.draw(ev)
    }
  }
}


object Scope {
  def apply(source: EventSource, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): Closeable =
    new ServerProxy(sv.linkStart(actorOf(new Scope(source)), lc)) with Closeable
}
