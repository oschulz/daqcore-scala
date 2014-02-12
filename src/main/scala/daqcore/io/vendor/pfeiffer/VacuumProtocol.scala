// Copyright (C) 2014 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.io.vendor.pfeiffer

import daqcore.io._
import daqcore.util._


object VacuumProtocol {

  object Action extends Enumeration {
    type Action = Value
    val ReadParam = Value(0)
    val WriteParam = Value(10)
  }
  import Action._


  trait Telegram {
    def addr: Int
    def action: Action
    def param: Int
    def data: ByteString
  }

  case class UniversalTel(addr: Int, action: Action, param: Int, data: ByteString) extends Telegram

  trait MasterTel extends Telegram
  trait SlaveTel extends Telegram

  case class CmdTel(addr: Int, param: Int, data: ByteString) extends MasterTel
    { def action = CmdRespTel.action }
  object CmdTel { val action = WriteParam }

  case class CmdRespTel (addr: Int, param: Int, data: ByteString) extends SlaveTel
    { def action = CmdRespTel.action }
  object CmdRespTel { val action = WriteParam }

  case class QryTel (addr: Int, param: Int) extends MasterTel
    { def action = QryTel.action; def data = QryTel.data }
  object QryTel
    { val action = ReadParam; val data = ByteString("=?", charset) }

  case class QryRespTel (addr: Int, param: Int, data: ByteString) extends SlaveTel
   { def action = CmdRespTel.action }
  object QryRespTel { val action = ReadParam }


  val charset = "ASCII"

   
  def string_n(len: Int) = new Codec[String, String] {
    def enc: Encoder[String] = (out: ByteStringBuilder, in: String) => {
      if (in.length > len) throw new IllegalArgumentException("Invalid string length for fixed length string")
      out ++= ByteString(in + ((1 to len - in.length) map { _ => " " } mkString ""), charset)
    }

    def dec: Decoder[String] = Decoder take len map { bytes => bytes.decodeString(charset) }
  }

  def u_integer_n(len: Int) = string_n(len).transform[Int, Int] {
      x =>
      if (x < 0) throw new IllegalArgumentException("Only non-negative values allowed")
      val s = x.toString
      if (s.length > len) throw new IllegalArgumentException("Integer doesn't fit into fixed string length")
      ((1 to len - s.length) map { _ => "0" } mkString "") + s
  } { _.toInt }


  val boolean_old = string_n(6).transform[Boolean, Boolean] {
    case true => "111111"
    case false => "000000"
  }{
    case "111111" => true
    case "000000" => false
    case _ => throw new RuntimeException("Invalid boolean_old")
  }

  val u_integer = u_integer_n(6)

  val u_real = u_integer.transform[Double, Double] { x => (x * 1e2).round.toInt } { _ / 1e2}

  val u_expo_new = new Codec[Double, Double] {
    def enc: Encoder[Double] = (out: ByteStringBuilder, x: Double) => {
      val exponent = if (x != 0) math.log10(x).floor.toInt else 0
      val mantissa = x / math.pow(10, exponent)
      u_integer_n(4).enc(out, (mantissa * 1e3).round.toInt)
      u_integer_n(2).enc(out, (exponent + 20).round.toInt)
    }

    def dec: Decoder[Double] = for {
      mantInt <- u_integer_n(4).dec
      expoInt <- u_integer_n(2).dec
    } yield mantInt / 1e3 * math.pow(10, expoInt - 20)
  }


  object TelegramCodec extends Codec[Telegram, Telegram] {
    import LineCodec.CR

    object FrameCodec extends Codec[ByteString, ByteString] {
      def checksum(bytes: ByteString): Byte =
        bytes.foldLeft(0.toByte){ (s,b) => (s+b).toByte }

      val enc: Encoder[ByteString] = (out: ByteStringBuilder, content: ByteString) => {
        out ++= content
        u_integer_n(3).enc(out, checksum(content))
        out ++= CR
      }

      def dec: Decoder[ByteString] = for { line <- Decoder takeUntil CR } yield {
        if (line.length < 3) throw new RuntimeException("Input frame too short")
        val (content, checksumBytes) = (line.dropRight(3), line.takeRight(3))
        u_integer_n(3)(checksum(content))
        if (u_integer_n(3)(checksum(content)) != checksumBytes)
          throw new RuntimeException("Checksum error")
        content
      }
    }

    object FrameContentCodec extends Codec[Telegram, Telegram] {
      val enc: Encoder[Telegram] = (out: ByteStringBuilder, tel: Telegram) => {
        u_integer_n(3).enc(out, tel.addr)
        u_integer_n(2).enc(out, tel.action.id)
        u_integer_n(3).enc(out, tel.param)
        u_integer_n(2).enc(out, tel.data.length)
        out ++= tel.data
      }

      def dec: Decoder[Telegram] = for {
        addr <- u_integer_n(3).dec
        action <- u_integer_n(2).dec map { id => Action(id) }
        param <- u_integer_n(3).dec
        dataLen <- u_integer_n(2).dec
        data <- Decoder take dataLen
      } yield UniversalTel(addr, action, param, data)
    }

    val enc: Encoder[Telegram] = (out: ByteStringBuilder, tel: Telegram) =>
      FrameCodec.enc(out, FrameContentCodec(tel))

    def dec: Decoder[Telegram] = for { tel <- FrameCodec.dec map { case FrameContentCodec(tel) => tel } }
      yield tel
  }

  object MasterTelCodec extends Codec[MasterTel, MasterTel] {
    val enc: Encoder[MasterTel] = TelegramCodec.enc
    def dec: Decoder[MasterTel] = for { tel <- TelegramCodec.dec } yield {
      tel.action match {
        case CmdTel.action => CmdTel(tel.addr, tel.param, tel.data)
        case QryTel.action if (tel.data == QryTel.data) => QryTel(tel.addr, tel.param)
        case _ => throw new RuntimeException("Invalid master telegram")
      }
    }
  }

  object SlaveTelCodec extends Codec[SlaveTel, SlaveTel] {
    val enc: Encoder[SlaveTel] = TelegramCodec.enc
    def dec: Decoder[SlaveTel] = for { tel <- TelegramCodec.dec } yield {
      tel.action match {
        case CmdRespTel.action => CmdRespTel(tel.addr, tel.param, tel.data)
        case QryRespTel.action => QryRespTel(tel.addr, tel.param, tel.data)
        case _ => throw new RuntimeException("Invalid slave telegram")
      }
    }
  }

}
