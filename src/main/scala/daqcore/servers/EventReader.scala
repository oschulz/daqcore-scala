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

import java.io.{OutputStream, File}
import java.util.UUID

import daqcore.util._
import daqcore.util.fileops._
import daqcore.actors._
import daqcore.profiles._
import daqcore.monads._
import daqcore.data._


trait EventInput extends GenericInput { val inputCompanion = EventInput }
object EventInput extends GenericInputCompanion { type InputData = AnyRef }


class EventReader(val source: SCPIRequestInput) extends InputFilterServer with EventInput {
  import daqcore.prot.scpi._
  
  val sourceCompanion = SCPIRequestInput
  
  var currentEvent: Option[raw.Event] = None
  var eventQueue: List[raw.Event] = Nil
  override def needMoreInput = currentEvent == None
  
  var transChannels: Seq[Int] = Nil
  var transTrigPos: Seq[Int] = Nil
  var transNSamples: Seq[Int] = Nil
  
  protected def srvProcessInput(data: Request) = {
    import daqcore.prot.scpi._
    import EventWriter.headers._
    
    trace("srvProcessInput(%s)".format(loggable(data)))

    object SeqCmd {
      def unapply(cmd: Command) = cmd match {
        case Command(header, args @ _*) => Some((header, args))
        case _ => None
      }
    }
    
    for {cmd <- data.instr} { trace(loggable(cmd)); cmd match {
      case Command(`H_FORMat`, SPD(format), SPD(version)) =>
        require( format == "DaqCoRE-Events-SCPI" && version == "0.1" )
    
      case Command(`H_EVENt`, NR1(idx), NRf(time)) => {
        require(currentEvent == None)
        currentEvent = Some(raw.Event(idx, time))
      }
      case SeqCmd(`H_EVEN_TRIGger`, NR1Seq(channels @ _*)) => {
        val ev = currentEvent.get
        currentEvent = Some( ev.copy(trig = ev.trig ++ channels) )
      }
      case SeqCmd(`H_EVENt_TRANSient_CHANnel`, NR1Seq(channels @ _*)) =>
        transChannels = channels
      case SeqCmd(`H_EVENt_TRANSient_TPOSition`, NR1Seq(trigPos @ _*)) =>
        transTrigPos = trigPos
      case SeqCmd(`H_EVENt_TRANSient_NSAMples`, NR1Seq(nSamples @ _*)) =>
        transNSamples = nSamples
      case SeqCmd(`H_EVENt_TRANSient_SAMples`, NR1Seq(allSamples @ _*)) => {
        require ( (transChannels.size > 0) && (transNSamples.sum == allSamples.size) )
        val channelIt = transChannels.iterator
        val trigPosIt = transTrigPos.iterator
        val nSamplesIt = transNSamples.iterator
        
        var transients = Map.empty[Int, raw.Transient]
        var offset = 0

        while(channelIt.hasNext) {
          val channel = channelIt.next
          val trigPos = trigPosIt.next
          val nSamples = nSamplesIt.next
          val samples = allSamples.slice(offset, offset + nSamples)
          transients = transients + (channel -> raw.Transient(trigPos, samples))
          offset = offset + nSamples
        }
        
        transChannels = Nil; transTrigPos = Nil; transNSamples = Nil
        val ev = currentEvent.get
        currentEvent = Some( ev.copy(trans = ev.trans ++ transients) )
      }
      case Command(`H_EVENt_END`) => {
        eventQueue = currentEvent.get::eventQueue
        currentEvent = None
      }
      case Command(`H_RUN_STARt`, SPD(uuidString), NRf(time)) =>
      case Command(`H_RUN_STOP`, NRf(duration)) =>

      case _ => throw new MatchError(loggable(cmd.toString))
    } }
    
    val result = eventQueue.reverse
    eventQueue = Nil
    result
  }
}


object EventReader {
  def apply(input: SCPIRequestInput): EventReader =
    start(new EventReader(input))
    
  def apply(file: File, compression: Compression = Uncompressed): EventReader =
    EventReader(SCPIRequestInputFilter(file, compression))
}
