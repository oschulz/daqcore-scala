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

import java.net.{SocketAddress, InetSocketAddress}

import akka.actor.{IO => AkkaIO, _}
import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future, Promise}

import daqcore.util._
import daqcore.actors._, daqcore.actors.TypedActorTraits._


trait CloseableResourceImpl extends CloseableTAImpl {
  val isOpenPromise = Promise[Boolean]()
  val isOpenFuture = isOpenPromise.future

  def setIsOpen(status: Boolean): Unit = isOpenPromise success status
  def isOpenOpt: Option[Boolean] = isOpenFuture.value match {
    case None => None
    case Some(Failure(error)) => throw error
    case Some(Success(v)) => Some(v)
  }

  override def close(): Unit = {
    if (isOpenOpt.isEmpty) setIsOpen(false)
    super.close()
  }
}


trait ByteStreamOutputImpl extends ByteStreamOutput with CloseableTAImpl
  with SyncableImpl
{
  val outputQueue = new ByteStringBuilder

  def flush(): Unit
  
  override def sync() = {
    flush()
    super.sync()
  }

  def send(data: ByteString) : Unit = if (! data.isEmpty) {
    outputQueue ++= data
    flush()
  }
  
  def send[A](data: A, encoder: Encoder[A]) : Unit = {
    encoder(outputQueue, data)
    flush()
  }
}

trait ByteStreamInputImpl extends ByteStreamInput with CloseableTAImpl {
  val inputQueue = DataDecoderQueue()

  def recv(): Future[ByteString] = recv(IO.takeAny)
  def recv(receiver: ActorRef, repeat: Boolean): Unit = recv(receiver, IO.takeAny, repeat)
  
  def recv[A](decoder: Decoder[A]): Future[A] = {
    val result = Promise[A]()
    inputQueue pushDecoder { decoder map { result success _ } }
    result.future
  }

  def recv(receiver: ActorRef, decoder: Decoder[_], repeat: Boolean): Unit = inputQueue pushDecoder {
    val action = decoder map { receiver ! _ }
    if (repeat) IO.repeat { action }
    else action
  }
}


trait ByteStreamIOImpl extends ByteStreamInputImpl with ByteStreamOutputImpl
  with ByteStreamIO
