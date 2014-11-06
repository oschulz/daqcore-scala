// Copyright (C) 2011-2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import akka.actor._

import daqcore.util._
import daqcore.io._
import daqcore.actors._, daqcore.actors.TypedActorTraits._

import collection.immutable.Queue


trait IseqXHQ extends PowerSupply {
  def query(cmd: String): Future[String]

  def getOutStatus(channels: Int*): Future[Seq[(Int, String)]]
  def getOutVoltStable(channels: Int*): Future[Seq[(Int, Boolean)]]
}


trait IseqNHQ extends IseqXHQ {
  def query(cmd: String): Future[String]
}

object IseqNHQ {
  def apply(aref: ActorRef)(implicit sys: ActorSystem) = typedActor[IseqNHQ](aref)

  def apply(io: ByteStreamIO)(implicit rf: ActorRefFactory): IseqNHQ =
    typedActorOf[IseqNHQ](new IseqNHQImpl(io))
}



abstract class IseqXHQImpl(io: ByteStreamIO) extends IseqNHQ
  with CloseableTAImpl with SyncableImpl
{
  import IseqXHQImpl._
  
  import daqcore.defaults.defaultTimeout

  protected def send(cmd: String) = {
    // Workaround for serial interface of some Iseq NHQ devices, which require a
    // long pause between each byte sent to them (else they will drop input bytes):
    val out = new ByteStringBuilder
    codec.enc(out, cmd)
    val bytes = out.result
    for (b <- bytes) {
      io.send(ByteString(b));
      io.getSync.get;
      Thread.sleep(100)
    }
  }
    
  protected def isegQry(cmd: String) = {
    send(cmd)

    val (replyA, replyB) = codec(io).recv.get
    
    if (replyA != cmd.trim) throw new RuntimeException("Command echo from iseq Supply does not match")
    if (replyB startsWith "?") replyB match {
      case iseqErrors.syntaxError => throw new IllegalArgumentException("iseq Supply: Syntax error in command \"%s\"".format(cmd))
      case iseqErrors.wrongChannel => throw new IllegalArgumentException("iseq Supply: Wrong channel in command \"%s\"".format(cmd))
      case iseqErrors.timeout => throw new RuntimeException("iseq Supply: Timeout error on command \"%s\"".format(cmd))
      case iseqErrors.voltageOverLimit(limit) => throw new IllegalArgumentException("iseq Supply: Voltage exceeds limit %s in command \"%s\"".format(limit, cmd))
      case error => throw new RuntimeException("iseq Supply:  Error(%s)".format(error))
    } else replyB
  }
  
  protected def iseqCommSync() {
    send("")
    io.recv(codec.decSync).get
  }
  
  iseqCommSync()
  
  protected def isegCmd(cmd: String): Unit = isegQry(cmd)
  
  def query(cmd: String) = successful(isegQry(cmd))
  
  // def isegQryDouble(cmd: String): Double = { val IsegDouble(x) = isegQry(cmd); x }

  protected def getChannels[A](cmd: String, channels: Seq[Int])(f: String => A): Seq[(Int, A)] =
    for (ch <- channels) yield { ch -> f(isegQry(cmd + ch)) }

  protected def setChannels[A](cmd: String, vals: Seq[(Int, A)])(f: A => String)(g: String => A): Seq[(Int, A)] = {
    for ((ch, x) <- vals) yield {
      isegCmd(cmd + ch + "=" + f(x))
      ch -> g(isegQry(cmd + ch))
    }
  }

  protected def getChannelsDouble(cmd: String, channels: Seq[Int]) =
    getChannels (cmd, channels) { s => val IsegDouble(x) = s; x}
    
  protected def setChannelsDouble(cmd: String, vals: Seq[(Int, Double)]) =
    setChannels (cmd, vals) {IsegDouble(_)} { s => val IsegDouble(x) = s; x}

  protected def getChannelsIntDouble(cmd: String, channels: Seq[Int]) =
    getChannels (cmd, channels) { _.toDouble }
    
  protected def setChannelsIntDouble(cmd: String, vals: Seq[(Int, Double)]) =
    setChannels (cmd, vals) { _.toInt.toString } { _.toDouble }

  protected def getIdentity() = isegQry("#")
  protected val idn = getIdentity()
  def identity = successful(idn)
  
  protected def checkConnection() {
    assert( getIdentity() == idn )
    scheduleOnce(15.seconds, selfRef, CheckConnection)
    log.trace("Connection checked")
  }

  checkConnection()

  protected val outs = {
    var nouts = 0
    try { while (nouts < 1000) { val ch = nouts + 1; isegCmd("S" + ch); nouts = ch } } catch { case e: IllegalArgumentException => }
    (1 to nouts)
  }
  def outputs = successful(outs)

  def getOutEnabled(channels: Int*) =
    successful(getChannels("S", channels){_.drop(3) != "OFF"})
  def setOutEnabled(vals: (Int, Boolean)*) = throw new UnsupportedOperationException("setOutEnabled not supported by device")

  def startVoltageChange(channels: Int*) = {
    for (ch <- channels) isegCmd("G" + ch)
  }
  
  def getOutVoltRiseRate(channels: Int*) = successful(getChannelsIntDouble("V", channels))
  def setOutVoltRiseRate(vals: (Int, Double)*) = successful(setChannelsIntDouble("V", vals))

  def getOutVoltFallRate(channels: Int*) = successful(getChannelsIntDouble("V", channels))
  def setOutVoltFallRate(vals: (Int, Double)*) = successful(setChannelsIntDouble("V", vals))

  def getOutStatus(channels: Int*) = successful(getChannels("S", channels){a => a})

  val outStateExpr="""S[0-9]=(.*)""".r
  def getOutVoltStable(channels: Int*): Future[Seq[(Int, Boolean)]] = successful (
      getChannels("S", channels) { s => s.trim match {
        case outStateExpr("ON") => true
        case outStateExpr("L2H") => false
        case outStateExpr("H2L") => false
        case outStateExpr("QUA") => false
        case _ => throw new RuntimeException("Can't reach stable output voltage: status code '%s', %s".format(s, s.getClass))
      }
    }
  )

  override def receive = extend(super.receive) {
    case CheckConnection => checkConnection()
  }
}


object IseqXHQImpl {
  protected case object CheckConnection

  object iseqErrors {
    val syntaxError = "????"
    val wrongChannel = "?WCN"
    val timeout = "?TOT"
    val voltageOverLimit = """[?] UMAX=(.*)""".r
  }

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
  
  object codec extends Codec[String, (String, String)] {
    val lineCodec = StringLineCodec(LineCodec.CRLF, "ASCII")

    val enc = lineCodec.enc

    val dec = for {
      a <- lineCodec.dec
      b <- lineCodec.dec
    } yield (a.trim, b.trim)

    val decSync = Decoder.takeUntil(ByteString("????\r\n"), true) map { _ => true}
  }
}



class IseqNHQImpl(io: ByteStreamIO) extends IseqXHQImpl(io) {
  import IseqXHQImpl._
  
  def getOutVoltDesired(channels: Int*) = successful(getChannelsIntDouble("D", channels))
  
  def setOutVoltDesired(vals: (Int, Double)*) = {
    val res = setChannelsIntDouble("D", vals)
    startVoltageChange((for ((ch, v) <- vals) yield ch): _*)
    successful(res)
  }

  def getOutVoltSensed(channels: Int*) = successful(getChannelsIntDouble("U", channels))
  def getOutCurrSensed(channels: Int*) = successful(getChannelsDouble("I", channels))

  def getOutCurrTrip(channels: Int*) = successful(getChannelsIntDouble("L", channels))
  def setOutCurrTrip(vals: (Int, Double)*) = successful(setChannelsIntDouble("L", vals))
}



class IseqSHQImpl(io: ByteStreamIO) extends IseqXHQImpl(io) {
  import IseqXHQImpl._
  
  def getOutVoltDesired(channels: Int*) = successful(getChannelsDouble("D", channels))
  
  def setOutVoltDesired(vals: (Int, Double)*) = {
    val res = setChannelsDouble("D", vals)
    startVoltageChange((for ((ch, v) <- vals) yield ch): _*)
    successful(res)
  }

  def getOutVoltSensed(channels: Int*) = successful(getChannelsDouble("U", channels))
  def getOutCurrSensed(channels: Int*) = successful(getChannelsDouble("I", channels))

  def getOutCurrTrip(channels: Int*): Future[Seq[(Int, Double)]] =
    throw new UnsupportedOperationException("getOutCurrTrip not implemented yet")

  def setOutCurrTrip(vals: (Int, Double)*): Future[Seq[(Int, Double)]]=
    throw new UnsupportedOperationException("setOutCurrTrip not implemented yet")
}
