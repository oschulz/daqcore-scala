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


package daqcore.servers

import scala.collection.immutable.{IntMap, SortedMap}
import scala.collection.MapLike

import java.io.{File}
import java.util.UUID

import daqcore.util._
import daqcore.actors._
import daqcore.profiles._
import daqcore.data._


class TTreeEventWriter(val source: EventSource, val target: File)(implicit timeout: TimeoutSpec) extends CloseableServer {
  val handler = EventHandler {
    case ev: RunStart => srv ! ev; true
    case ev: RunStop => srv ! ev; true
    case ev: raw.Event => srv ! ev; true
  }
  
  var rio: RootIO = null
  var outFile: TFile = null
  var events: TTree[raw.FlatEvent] = null
  var runs: TTree[RunInfo] = null
  var runStart: Option[RunStart] = None
  

  override def init() = {
    super.init()
    withCleanup {source.addHandler(handler)} {source.removeHandler(handler)}
    
    rio = RootIO(); link(rio.srv); addResource(rio)
    
    outFile = rio.openTFile(target, filemode.recreate)(timeout)
    addResource(outFile)
    events = outFile.createTTree[raw.FlatEvent]("events", "Events")
    runs = outFile.createTTree[RunInfo]("runs", "Run Information")
  }


  override def serve = super.serve orElse {
    case event: raw.Event => {
      trace(loggable(event))
      for { start <- runStart } require(event.run == start.uuid)
      events += raw.FlatEvent(event)
    }
    case start: RunStart => {
      debug(start)
      runStart = Some(start)
    }
    case stop: RunStop => {
      debug(stop)
      runs += new RunInfo(runStart.get, stop)
      runStart = None
    }
    case Closeable.Close => {
      exit('closed)
    }
  }
}


object TTreeEventWriter {
  def apply(source: EventSource, output: File)(implicit timeout: TimeoutSpec): TTreeEventWriter =
    start(new TTreeEventWriter(source, output)(timeout))
}
