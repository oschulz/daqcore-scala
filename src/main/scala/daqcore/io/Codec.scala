// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.io

import scala.concurrent.Future
import scala.concurrent.duration._

import daqcore.util._


trait Codec[A, B] {
  def enc: Encoder[A]
  def dec: Decoder[B]

  def apply(stream: ByteStreamIO): TypedIO[A, B] = TypedIO(this, stream)

  def apply(values: A*): ByteString = {
    val bld = ByteStringBuilder()
    values foreach { v => enc(bld, v) }
    bld.result()
  }
  
  def unapplySeq(bytes: ByteString) = try {
    val buffer = collection.mutable.ListBuffer[B]()
    val initial = for { elem <- dec } yield { buffer += elem; {} }
    var state: (IO.Iteratee[Unit], ByteString) = (initial, bytes)
    while ( state match { case (_, b) if b.isEmpty => false; case _ => true } ) {
      state = state._1(state._2)
      state match {
        case (_: IO.Done[_], rest) => state = (initial, rest)
        case (cont: IO.Next[_], _) => throw new IllegalArgumentException("Unexpected EOI")
        case (IO.Failure(cause), _) => throw cause
      }
    }
    Some(buffer.result)
  } catch {
    case e: Throwable => None
  }
}

object Codec {
  def apply[A, B](enc: Encoder[A], dec: Decoder[B]): Codec[A, B] =
    SimpleCodec(enc, dec)
}

case class SimpleCodec[A, B] (enc: Encoder[A], dec: Decoder[B]) extends Codec[A, B]


case class TypedIO[A, B](codec: Codec[A, B], stream: ByteStreamIO) {
  def send(data: A): Unit = stream.send(data, codec.enc)

  def recv(): Future[B] = stream.recv(codec.dec)

  def flush(): Unit = stream.flush()

  def sync() = stream.sync()

  def getSync() = stream.getSync()
  
  def pause(duration: Duration) = stream.pause(duration)
}



object LineCodec {
  val CR = ByteString("\r")
  val LF = ByteString("\n")
  val CRLF = ByteString("\r\n")
}


case class StringLineCodec(separator: ByteString = LineCodec.LF, charset: String = "UTF-8") extends Codec[String, String] {
  val enc: Encoder[String] = (out: ByteStringBuilder, in: String) => {
    val bs = ByteString(in, charset)
	out ++= bs
	if (! bs.endsWith(separator)) out ++= separator
  }
  
  val dec: Decoder[String] = IO takeUntil separator map { _.decodeString(charset) }
}


case class RawLineCodec(separator: ByteString = LineCodec.LF) extends FrameCodec {
  val enc: Encoder[ByteString] = (out: ByteStringBuilder, bs: ByteString) => {
	out ++= bs
	if (! bs.endsWith(separator)) out ++= separator
  }
  
  val dec: Decoder[ByteString] = IO takeUntil separator
}
