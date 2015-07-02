// Copyright (C) 2013 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import daqcore.actors._
import daqcore.io._
import daqcore.util._


trait PfeifferTPG261 extends Device {
  def cmd(req: String): Future[Unit]
  def qry(req: String): Future[String]

  def readPressure(ch: Ch = Ch(1)): Future[ChV[Double]] 
}

object PfeifferTPG261 extends DeviceCompanion[PfeifferTPG261] {
  def impl = { case uri => new PfeifferTPG261Impl(uri.toString) }
}



class PfeifferTPG261Impl(ioURI: String) extends PfeifferTPG261
  with CloseableTAImpl with SyncableImpl
{
  import PfeifferTPG261Impl._

  import daqcore.defaults.defaultTimeout
  implicit def executor = defaultExecContext

  protected val nChannels = 1

  protected val codec = StringLineCodec(LineCodec.CR, "ASCII")
  protected val io = ByteStreamIO(ioURI, "io")

  def cmd(req: String): Future[Unit] = {
    io.send(req, encCmd)
    val r = io.recv(decCmdAck) map {
      case true => ()
      case false => throw new RuntimeException("Received NACK from device")
    }
    r.get; r // Block until ACK, device can't handle overlapping commands
  }

  def qry(req: String): Future[String] = {
    cmd(req)
    io.send(ENQ)
    val r = io.recv(decEnqResp)
    r.get; r // Block until ACK, device can't handle overlapping commands
  }


  def identity = successful("PfeifferTPG261")

  def readPressure(channels: Ch): Future[ChV[Double]] = {
    if ((channels.min < 1) || (channels.max > nChannels)) throw new IllegalArgumentException("Invalid channel number")
	val fts = channels.toSeq map { i => qry("PR%s".format(i)) }
    Future.sequence(fts) map { xs => ChV(xs) { case (i,v) => (i+1, v.split(",")(1).toDouble) } }
  }
}


object PfeifferTPG261Impl {
  import daqcore.io.LineCodec.{CR, LF, CRLF}
  
  val charset = "ASCII"

  val ETX  = ByteString(0x03)
  val ENQ  = ByteString(0x05)
  val ACK  = ByteString(0x06)
  val NACK = ByteString(0x15)
  val ACKMsg = ACK ++ CRLF
  val NACKMsg = NACK ++ CRLF

  val encCmd: Encoder[String] = (out: ByteStringBuilder, in: String) => {
	out ++= ByteString(in, charset)
	out ++= CRLF
  }
  
  val decCmdAck: Decoder[Boolean] = Decoder take 3 map {
    case ACKMsg => true
    case NACKMsg => false
    case _ => throw new RuntimeException ("Unexpected reply to command")
  }

  val decEnqResp: Decoder[String] = Decoder takeUntil CRLF map { _.decodeString(charset) }
}
