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
import org.jfree.chart._

import daqcore.util._


class SwingChartPanel(chart: JFreeChart) extends Panel {
  override lazy val peer = {
    val panel = new org.jfree.chart.ChartPanel(chart)
    panel.setMouseWheelEnabled(true)
    panel
  }
}

object SwingChartPanel {
  def apply(plot: JFreeChart) = new SwingChartPanel(plot)
}



class SwingChartFrame(chart: JFreeChart, frameTitle: String = "Chart") extends Frame {
  title = frameTitle
  contents = SwingChartPanel(chart)
  
  def show() = Swing.onEDT {
    pack()
    visible = true
  }
  
  show()
}


object SwingChartFrame {
  def apply(chart: JFreeChart, frameTitle: String = "Chart") = new SwingChartFrame(chart, frameTitle)
}
