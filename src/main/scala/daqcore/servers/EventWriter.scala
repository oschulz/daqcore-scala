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
import scala.actors._

import java.io.{OutputStream, File}
import java.util.UUID

import daqcore.util._
import daqcore.util.fileops._
import daqcore.actors._
import daqcore.profiles._
import daqcore.monads._
import daqcore.data.raw._


class EventWriter(output: OutputStream) extends Server with Closeable {
  import daqcore.prot.scpi._

  val CHANnel = Mnemonic("CHANnel")
  val TRANSient = Mnemonic("TRANSient")
  val TPOSition = Mnemonic("TPOSition")
  val NSAMples = Mnemonic("NSAMples")
  val SAMples = Mnemonic("SAMples")


  protected def write(msg: Message): Unit = {
    output.write(msg.charSeq.toArray)
    output.write(StreamMsgTerm.toArray)
  }

  protected def write(instr: Instruction): Unit =
    write(Request(instr))

  protected def flush(): Unit = output.flush()

  
  protected def writeImpl(event: Event): Unit = {
    import daqcore.prot.scpi.mnemonics._
    write( ~EVENt!( NR1(event.idx), NRf(event.time)) )
    write( ~EVEN~TRIGger!(event.trig map {t => NR1(1)}: _*) )
    write( ~EVENt~TRANSient~CHANnel!(event.trans map {tr => NR1(tr._1)} toSeq: _*) )
    write( ~EVENt~TRANSient~TPOSition!(event.trans map {tr => NR1(tr._2.trigPos)} toSeq: _*) )
    write( ~EVENt~TRANSient~NSAMples!(event.trans map {tr => NR1(tr._2.samples.size)} toSeq: _*) )
    write( ~EVENt~TRANSient~SAMples!((for {(ch,tr) <- event.trans; s <-tr.samples} yield NR1(s)) toSeq: _*) )
    write( ~EVENt~END! )
  }

  {
    import daqcore.prot.scpi.mnemonics._
    write(~FORMat!(SPD("DaqCoRE-Events-SCPI"), SPD("0.1")))
  }

  override def init() = {
    withCleanup{}{ flush() }
  }


  def serve = {
    case event: Event => writeImpl(event)
    case op @ RunStart(uuid, time) => {
      import daqcore.prot.scpi.mnemonics._
      debug(op)
      write(~RUN~STARt!(SPD(uuid.toString), NRf(time)))
    }
    case op @ RunStop(duration) => {
      import daqcore.prot.scpi.mnemonics._
      debug(op)
      write(~RUN~STOP!(NRf(duration)))
      flush()
    }
    case op @ Sync() => {
      debug(op)
      reply(Ok(true))
    }
    case Closeable.Close => {
      output.close()
      exit('closed)
    }
  }

  case class Sync()
  case class RunStart(uuid: UUID = UUID.randomUUID(), time: Double = currentTime)
  case class RunStop(duration: Double)
  
  def write(event: Event): Unit = srv ! event
  
  def sync(): Unit = srv.!!^[MaybeFail[Boolean]](Sync()).apply().get
  
  def runStart(uuid: UUID = UUID.randomUUID(), time: Double = currentTime): Unit =
    srv ! RunStart(uuid, time)

  def runStop(duration: Double): Unit = srv ! RunStop(duration)
}


object EventWriter {
  def apply(output: OutputStream): EventWriter =
    start(new EventWriter(output))
    
  def apply(file: File): EventWriter =
    EventWriter(file.getOStream)
}