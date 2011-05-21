// Copyright (C) 2011 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.devices

import akka.actor.Actor.actorOf, akka.actor.ActorRef, akka.dispatch.Future

import daqcore.util._
import daqcore.actors._
import daqcore.io._
import daqcore.io.prot.snmp._
import akka.dispatch.Future

import collection.immutable.Queue


trait IseqNHQ extends PowerSupply

object IseqNHQ {
  def apply(aref: ActorRef) = SReqProxy[IseqNHQ](aref)

  def apply(msgLnk: RawMsgIO, sv: Supervising = defaultSupervisor): IseqNHQ =
    this(sv.linkStart(new IseqNHQSrv(msgLnk)))
}



class IseqNHQSrv(msgLnk: RawMsgIO) extends MServer {
  import IseqNHQSrv._

  def isegQry(cmd: String) = {
    msgLnk.send(ByteCharSeq(cmd + "\r\n"))

    val Seq(replyA, replyB) = Future.traverse(Seq(msgLnk, msgLnk)){_.recvF()}.get map { b => ByteCharSeq(b: _*).toString.trim}
    assert(replyA == cmd.trim)
    if (replyB startsWith "?") replyB match {
      case iseqErrors.syntaxError => throw new IllegalArgumentException("iseq Supply: Syntax error in command \"%s\"".format(cmd))
      case iseqErrors.wrongChannel => throw new IllegalArgumentException("iseq Supply: Wrong channel in command \"%s\"".format(cmd))
      case iseqErrors.timeout => throw new RuntimeException("iseq Supply: Timeout error on command \"%s\"".format(cmd))
      case iseqErrors.voltageOverLimit(limit) => throw new IllegalArgumentException("iseq Supply: Voltage exceeds limit %s in command \"%s\"".format(limit, cmd))
      case error => throw new RuntimeException("iseq Supply:  Error(%s)".format(error))
    } else replyB
  }
  
  def isegCmd(cmd: String): Unit = isegQry(cmd)

  object IsegDouble {
    val isegExpNotation = """([+-]?.*)([+-].*)""".r
    
    def apply(x: Double) = x.toString
    
    def unapply(s: String): Option[Double] = s match {
      case isegExpNotation(significand, exponent) =>
        try { Some( (significand + "e" + exponent).toDouble ) }
        catch { case e: NumberFormatException => None }
      case _ => None
    }
  }
  
  // def isegQryDouble(cmd: String): Double = { val IsegDouble(x) = isegQry(cmd); x }

  def getChannels[A](cmd: String)(f: String => A): Seq[(Int, A)] =
    for (ch <- outputs) yield { ch -> f(isegQry(cmd + ch)) }

  def setChannels[A](cmd: String, vals: Seq[(Int, A)])(f: A => String)(g: String => A): Seq[(Int, A)] = {
    for ((ch, x) <- vals) yield {
      isegCmd(cmd + ch + "=" + f(x))
      ch -> g(isegQry(cmd + ch))
    }
  }

  def getChannelsDouble(cmd: String) =
    getChannels (cmd) { s => val IsegDouble(x) = s; x}
    
  def setChannelsDouble(cmd: String, vals: Seq[(Int, Double)]) =
    setChannels (cmd, vals) {IsegDouble(_)} { s => val IsegDouble(x) = s; x}

  def getChannelsIntDouble(cmd: String) =
    getChannels (cmd) { _.toDouble }
    
  def setChannelsIntDouble(cmd: String, vals: Seq[(Int, Double)]) =
    setChannels (cmd, vals) { _.toInt.toString } { _.toDouble }


  def getIdentity() = isegQry("#")
  @sreq val identity: String = getIdentity()
  
  def triggerConnectionCheck() { sendAfter(15000, self, CheckConnection()) }
  @msg def checkConnection(check: CheckConnection) {
    assert( getIdentity() == identity )
    triggerConnectionCheck()
    log.trace("Connection checked")
  }
  triggerConnectionCheck()
  
  @sreq val outputs: Seq[Int] = {
    var nouts = 0
    try { while (nouts < 1000) { val ch = nouts + 1; isegCmd("S" + ch); nouts = ch } } catch { case e: IllegalArgumentException => }
    (1 to nouts)
  }

  @sreq def getOutEnabled(): Seq[(Int, Boolean)] =
    getChannels("S"){_.drop(3) != "OFF"}
  // @sreq def setOutEnabled(vals: (Int, Boolean)*) // Unsupported by device

  @sreq def getOutVoltDesired(): Seq[(Int, Double)] = getChannelsDouble("D")
  
  @sreq def setOutVoltDesired(vals: (Int, Double)*): Seq[(Int, Double)] = {
    val res = setChannelsDouble("D", vals)
    for ((ch, v) <- vals) isegCmd("G" + ch)
    res
  }

  @sreq def getOutVoltRiseRate(): Seq[(Int, Double)] = getChannelsIntDouble("V")
  @sreq def setOutVoltRiseRate(vals: (Int, Double)*) = setChannelsIntDouble("V", vals)

  @sreq def getOutVoltFallRate(): Seq[(Int, Double)] = getChannelsIntDouble("V")
  @sreq def setOutVoltFallRate(vals: (Int, Double)*) = setChannelsIntDouble("V", vals)

  @sreq def getOutVoltSensed(): Seq[(Int, Double)] = getChannelsDouble("U")
}


object IseqNHQSrv {
  object iseqErrors {
    val syntaxError = "????"
    val wrongChannel = "?WCN"
    val timeout = "?TOT"
    val voltageOverLimit = """[?] UMAX=(.*)""".r
  }
  
  protected case class CheckConnection()
}
