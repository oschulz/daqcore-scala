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

import akka.actor._, akka.actor.Actor._
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}

import daqcore.util._
import daqcore.util.fileops._
import daqcore.actors._
import daqcore.profiles._
import daqcore.monads._
import daqcore.data._


class SCPIEventWriter(val source: EventSource, val output: SCPIRequestOutput) extends CloseableServer {
  import daqcore.prot.scpi._
  import SCPIEventWriter.headers._

  override def profiles = super.profiles.+[Closeable]
  
  val handler = EventHandler {
    case ev: RunStart => srv ! ev; true
    case ev: RunStop => srv ! ev; true
    case ev: raw.Event => srv ! ev; true
  }
  
  var startState = RunStart()
  
  def write(request: Request): Unit =
    output.send(request)

  def write(instr: Instruction): Unit =
    write(Request(instr))

  def flush(): Unit = output.flush()

  {
    import daqcore.prot.scpi.mnemonics._
    write(H_FORMat!(SPD("DaqCoRE-Events-SCPI"), SPD("0.1")))
  }

  override def init() = {
    super.init()
    clientLinkTo(source.srv)
    withCleanup {source.addHandler(handler)} {source.removeHandler(handler)}
    clientLinkTo(output.srv)
    atCleanup { output.close() }
  }

  override def onServerExit(server: ActorRef, reason: Option[Throwable]) = {
    if ((server == source.srv) && (reason == None)) self.stop()
    else super.onServerExit(server, reason)
  }

  override def serve = super.serve orElse {
    case event: raw.Event => {
      import daqcore.prot.scpi.mnemonics._
      trace(loggable(event))
      val transCh = event.trans.keys.toSeq.sortWith{_ < _}
      write( H_EVENt!( NR1(event.idx), NRf(event.time)) )
      write( H_EVEN_TRIGger!(event.trig map {t => NR1(t)}: _*) )
      write( H_EVENt_TRANSient_CHANnel!((for (ch <- transCh.view) yield NR1(ch)): _*) )
      write( H_EVENt_TRANSient_TPOSition!((for (ch <- transCh.view) yield NR1(event.trans(ch).trigPos)): _*) )
      write( H_EVENt_TRANSient_NSAMples!((for (ch <- transCh.view) yield NR1(event.trans(ch).samples.size)): _*) )
      write( H_EVENt_TRANSient_SAMples! ((for {ch <- transCh.view; s <-event.trans(ch).samples.view} yield NR1(s)): _*) )
      write( H_EVENt_END! )
    }
    case op @ RunStart(_, uuid, time) => {
      import daqcore.prot.scpi.mnemonics._
      debug(op)
      write(H_RUN_STARt!(SPD(uuid.toString), NRf(time)))
    }
    case op @ RunStop(duration) => {
      import daqcore.prot.scpi.mnemonics._
      debug(op)
      write(H_RUN_STOP!(NRf(duration)))
      flush()
    }
  }
}


object SCPIEventWriter {
  object mnemonics {
    import daqcore.prot.scpi._
    val CHANnel = Mnemonic("CHANnel")
    val TRANSient = Mnemonic("TRANSient")
    val TPOSition = Mnemonic("TPOSition")
    val NSAMples = Mnemonic("NSAMples")
    val SAMples = Mnemonic("SAMples")
  }
  
  object headers {
    import daqcore.prot.scpi.mnemonics._
    import SCPIEventWriter.mnemonics._
    
    val H_FORMat = ~FORMat

    val H_EVENt = ~EVENt
    val H_EVEN_TRIGger = ~EVENt~TRIGger
    val H_EVENt_TRANSient_CHANnel = ~EVENt~TRANSient~CHANnel
    val H_EVENt_TRANSient_TPOSition = ~EVENt~TRANSient~TPOSition
    val H_EVENt_TRANSient_NSAMples = ~EVENt~TRANSient~NSAMples
    val H_EVENt_TRANSient_SAMples = ~EVENt~TRANSient~SAMples
    val H_EVENt_END = ~EVENt~END

    val H_RUN_STARt = ~RUN~STARt
    val H_RUN_STOP = ~RUN~STOP
  }
  
  def apply(source: EventSource, output: SCPIRequestOutput, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): Closeable =
    new ServerProxy(sv.linkStart(actorOf(new SCPIEventWriter(source, output)), lc)) with Closeable
}
