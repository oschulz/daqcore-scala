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

import akka.actor._, akka.actor.Actor._, akka.dispatch.Future
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle, Temporary, OneForOneStrategy, AllForOneStrategy}

import daqcore.util._
import daqcore.actors._
import daqcore.profiles._
import daqcore.data._


class TTreeEventWriter(val source: EventSource, val target: File, timeout: Long = 10000) extends PostInit with CloseableServer {
  override def profiles = super.profiles.+[Closeable]
  
  val handler = EventHandler {
    case ev: RunStart => srv ! ev; true
    case ev: RunStop => srv ! ev; true
    case ev: raw.Event => srv ! ev; true
  }
  
  var rio: RootIO = _
  var outFile: Option[TFile] = None
  var events: Option[TTree[raw.FlatEvent]] = None
  var runs: Option[TTree[RunInfo]] = None
  var runStart: Option[RunStart] = None
  
  def dateString(unixTime: Double) = {
    import java.util.{Date,TimeZone}, java.text.SimpleDateFormat
    // val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")
    val dateFormat = new SimpleDateFormat("yyyyMMdd_HHmmss")
    dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"))
    val unixMillis = (unixTime * 1e3).toLong
    dateFormat.format(new Date(unixMillis))
  }

  override def init() = {
    super.init()
    
    self.lifeCycle = Temporary // Restarting would be problematic with unfinished TFile
    
    clientLinkTo(source.srv)
    withCleanup {source.addHandler(handler)} {source.removeHandler(handler)}
  }

  override def postInit() = {
    super.postInit()
    
    rio = RootIO(srv, Temporary)
  }

  override def onServerExit(server: ActorRef, reason: Option[Throwable]) = {
    if ((server == source.srv) && (reason == None) && (runStart == None)) self.stop()
    else super.onServerExit(server, reason)
  }

  override def serve = super.serve orElse {
    case event: raw.Event => {
      trace(loggable(event))
      for { start <- runStart } require(event.run == start.uuid)
      events match {
        case Some(events) => events += raw.FlatEvent(event)
        case None => log.warn("No output open, can't write event")
      }
    }
    case start: RunStart => {
      debug(start)
      runStart = Some(start)
      val uuidStamp = start.uuid.toString.take(8)
      val timeStamp = dateString(start.startTime)
      val file = new java.io.File(target.getPath + "_%s_%s.root".format(timeStamp, uuidStamp))
      outFile = Some(rio.openTFile(file, filemode.recreate, timeout))
      events = Some(outFile.get.createTTree[raw.FlatEvent]("events", "Events"))
      runs = Some(outFile.get.createTTree[RunInfo]("runs", "Run Information"))
    }
    case stop: RunStop => {
      debug(stop)
      runStart match {
        case Some(start) => runs.get += new RunInfo(start, stop)
        case None => log.warn("No run start info available")
      }
      runStart = None

      outFile match {
        case Some(file) => {      
          log.debug("Closing output file")
          file.close()
        }
        case None => log.debug("No open file to close")
      }
    }
  }
}


object TTreeEventWriter {
  def apply(source: EventSource, output: File, timeout: Long = 10000, sv: Supervising = defaultSupervisor): Closeable =
    new ServerProxy(sv.linkStart(actorOf(new TTreeEventWriter(source, output, timeout)))) with Closeable
}
