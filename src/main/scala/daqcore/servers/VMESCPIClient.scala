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

import org.acplt.oncrpc.OncRpcProtocols

import daqcore.oncrpc.vxi11core
import daqcore.actors._
import daqcore.profiles._
import daqcore.util._
import daqcore.monads._

import daqcore.prot.scpi._, daqcore.prot.scpi.mnemonics._


class VMESCPIClient(dev: SCPIClientLink) extends Server with VMEBus {
  val VME = Mnemonic("VME")
  val defaultTimeout = 60000
  
  protected case class Fwd[T](target: MsgTarget, op: T)
  
  class ReadQueue extends Server {
    protected def unexpectedResponse: PartialFunction[Response, Unit] = { case resp =>
      error("Unexpected SCPI response: " + resp)
      exit('unexpectedResponse)
    }
  
    protected def query(instr: Instruction*)(body: PartialFunction[Response, Unit]) = {
      for (optRes <- dev.queryF(defaultTimeout, instr: _*)) {
        optRes match {
          case Some(response) => {
            trace("Processing response: " + response)
            (body orElse unexpectedResponse)(response)
          }
          case None => {
            trace("Read timed out")
            exit(Timeout)
          }
        }
      }
    }

    override protected def init() = {
      super.init()
      link(dev.srv)
    }
    
    protected def serve = {
      case op @ Fwd(repl, MemoryLink.Read(address, count)) => {
        trace(op)
        query(~VME~READ?(NR1(address.toInt), NR1(count.toInt))) {
          case Response(Result(value)) => value match {
            case BlockData(bytes) => {
              trace("Read data: " + bytes)
              repl ! bytes
              trace("Replied read result to sender")
            }
            case value => {
              error("Block Data expected: " + value.toList)
              exit('unexpectedResponse)
            }
          }
        }
      }
      case op @ MemoryLink.Pause() => {
        dev.cmd(WAI!)
        reply()
      }
      case op @ MemoryLink.Sync() => {
        val repl = sender
        query(WAI!, ESR?) {
          case Response(Result(NR1(esr))) =>
            val esrErrorMask = 0x3c;
            val result =
              if ((esr & esrErrorMask) == 0) Ok(true)
              else Fail(new RuntimeException("SCPI ESR returned " + esr))
            repl ! result
        }
      }
      case _ => exit('unknownMessage)
    }
  }
  val readQueue = new ReadQueue

  override def init() = {
    link(dev.srv)
    link(readQueue)
    readQueue.startOrRestart()
  }


  def serve = {
    case op: MemoryLink.Read => {
      trace(op)
      readQueue.forward(Fwd(sender, op))
    }
    case op @ MemoryLink.Write(address, bytes) => {
      trace(op)
      dev.cmd(~VME~WRITe!(NR1(address.toInt), BlockData(bytes.toIndexedSeq)))  //!! toIndexedSeq not optimal
    }
    case op @ MemoryLink.Pause() => {
      trace(op)
      readQueue.!?>(op) { case _ =>
        trace("%s executed" format op)
      }
    }
    case op @ MemoryLink.Sync() => {
      trace(op)
      val repl = sender
      val a = readQueue.!?>(op) { case a =>
        trace("%s returning %s".format(op, a))
        repl ! a 
      }
    }
    case Closeable.Close => {
      dev.close()
      exit('closed)
    }
  }
}


object VMESCPIClient {
  def apply (dev: SCPIClientLink) : VMESCPIClient =
    start(new VMESCPIClient(dev))
  
  def apply (host: String, port: Int) : VMESCPIClient =
    VMESCPIClient(SCPIClientLink(host, port))
}
