// Copyright (C) 2010-2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import akka.actor._
import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future, Promise}

import daqcore.util._
import daqcore.actors._, daqcore.actors.TypedActorTraits._


trait ByteFrameOutputImpl extends ByteFrameOutput with CloseableTAImpl
  with SyncableImpl
{
  val outputQueue = new ByteStringBuilder

  def send[A](data: A, encoder: Encoder[A]) : Unit = {
    val bld = ByteStringBuilder()
    encoder(bld, data)
    send(bld.result())
  }
}



trait ByteFrameInputImpl extends ByteFrameInput with CloseableTAImpl {
  val inputQueue = new DataActionQueue[ByteString]

  protected def decode[A](data: ByteString, decoder: Decoder[A]) = {
    var result: Option[A] = None

    val dec = decoder map { x => result = Some(x) }
    val (next, rest) = dec(data)
    next match {
      case Decoder.Done(_) => // Nothing to do
      case cont: Decoder.Next[_] => throw new RuntimeException("EOI during frame decoding")
      case Decoder.Failure(cause) => throw cause
    }
    if (!rest.isEmpty) throw new RuntimeException("Data rest after frame decoding")

    result.get
  }


  def recv() = {
    val recvPromise = Promise[ByteString]
    inputQueue.pushAction(recvPromise success _)
    recvPromise.future
  }


  def recv(receiver: ActorRef, repeat: Boolean) = {
    inputQueue.pushAction { data =>
      if (repeat) recv(receiver, repeat);
      receiver ! data
    }
  }

  
  def recv[A](decoder: Decoder[A]): Future[A] =
    recv() map { data => decode(data, decoder) }


  def recv(receiver: ActorRef, decoder: Decoder[_], repeat: Boolean): Unit = {
    inputQueue.pushAction { data =>
      if (repeat) recv(receiver, repeat);
      receiver ! decode(data, decoder)
    }
  }
}



trait ByteFrameIOImpl extends ByteFrameInputImpl with ByteFrameOutputImpl
  with ByteFrameIO
