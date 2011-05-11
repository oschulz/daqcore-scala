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

import java.io.IOException
import java.net.InetAddress
import java.util.concurrent.TimeoutException

import akka.actor._, akka.actor.Actor._
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle, Temporary}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._

import daqcore.prot.scpi._, daqcore.prot.scpi.mnemonics._


class VMESCPIClient(dev: SCPIClientLink, timeout: Long) extends CascadableServer with KeepAlive with PostInit {
  override def profiles = super.profiles.+[VMEBus]

  val VME = Mnemonic("VME")
  
  case class Fwd[T](target: MsgTarget, op: T)
  
  case class RQPause() extends ActorQuery[Unit]
  
  class ReadQueue extends Server {
    def unexpectedResponse: PartialFunction[Response, Unit] = { case resp =>
      throw new RuntimeException("Unexpected SCPI response: " + resp)
    }
  
    def query(instr: Instruction*)(body: PartialFunction[Response, Unit]) = {
      val response = (dev.queryF(instr: _*)(timeout)).get
      trace("Processing response: " + response)
      (body orElse unexpectedResponse)(response)
    }

    def serve = {
      case op @ Fwd(repl, VMEBus.Read(address, count, mode)) => {
        trace(op)
        query(~VME~READ?(NR1(mode.space.id), NR1(mode.width.id), NR1(mode.cycle.id), NR1(address.toInt), NR1(count.toInt))) {
          case Response(Result(value)) => value match {
            case BlockData(bytes) => {
              trace("Read data: " + bytes)
              repl ! bytes
              trace("Replied read result to sender")
            }
            case value => {
              throw new RuntimeException("Block Data expected: " + loggable(value.toList))
            }
          }
        }
      }
      case op @ RQPause() => {
        dev.cmd(WAI!)
        reply()
      }
      case op @ VMEBus.Sync() => {
        val repl = replyTarget
        query(WAI!, ESR?) {
          case Response(Result(NR1(esr))) =>
            val esrErrorMask = 0x3c;
            if ((esr & esrErrorMask) == 0) reply()
            else throw(new RuntimeException("SCPI ESR returned " + esr))
        }
      }
    }
  }

  var readQueue: ActorRef = _


  override def init() = {
    super.init()
    clientLinkTo(dev.srv)
    atShutdown(dev.srv.stop())
  }


  override def postInit() = {
    super.postInit()
    readQueue = srv.linkStart(actorOf(new ReadQueue), Temporary)
  }


  override def serve = super.serve orElse {
    case op: VMEBus.Read => {
      trace(op)
      readQueue.forward(Fwd(replyTarget, op))
    }
    case op @ VMEBus.Write(address, bytes, mode) => {
      trace(op)
      dev.cmd(~VME~WRITe!(NR1(mode.space.id), NR1(mode.width.id), NR1(mode.cycle.id), NR1(address.toInt), BlockData(bytes)))  //!! toIndexedSeq not optimal
    }
    case op @ VMEBus.Pause() => {
      trace(op)
      readQueue.!>(RQPause())
      trace("%s executed" format op)
    }
    case op @ VMEBus.Sync() => {
      trace(op)
      val repl = replyTarget
      val a = readQueue.!>(op)
      trace("%s returning %s".format(op, a))
      repl ! a 
    }
  }
}


object VMESCPIClient {
  def apply(dev: SCPIClientLink, timeout: Long = 10000, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): VMEBus =
    new ServerProxy(sv.linkStart(actorOf(new VMESCPIClient(dev, timeout)), lc)) with VMEBus
  
  def overInetStream(addr: InetSockAddr, timeout: Long = 10000, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): VMEBus = {
    val dev = SCPIClientLink.overInetStream(addr, timeout, sv, lc)
    VMESCPIClient(dev, timeout, sv, lc)
  }
}
