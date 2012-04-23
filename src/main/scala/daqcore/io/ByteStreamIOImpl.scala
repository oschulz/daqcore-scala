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

import akka.actor._
import akka.dispatch.{Future, Promise}
import akka.util.{ByteString, Duration, Timeout}
import akka.util.duration._

import daqcore.util._
import daqcore.actors._, daqcore.actors.TypedActorTraits._


trait FooBarxvc extends CloseableTAImpl {
  val isOpenPromise = Promise[Boolean]()
}

trait ByteStreamIOImpl extends ByteStreamIO with CloseableTAImpl
  with SyncableImpl
{
  val inputQueue = DataDecoderQueue()
  val outputQueue = new ByteStringBuilder

  val isOpenPromise = Promise[Boolean]()

  def setIsOpen(status: Boolean): Unit = isOpenPromise success status
  def isOpenOpt: Option[Boolean] = isOpenPromise.value match {
    case None => None
    case Some(Left(error)) => throw error
    case Some(Right(v)) => Some(v)
  }

  def flush(): Unit
  
  override def sync() = {
    flush()
    super.sync()
  }

  def recv(): Future[ByteString] = recv(IO takeAny)
  def recv(receiver: ActorRef, repeat: Boolean): Unit = recv(receiver, IO takeAny, repeat)
  
  def send(data: ByteString) : Unit = if (! data.isEmpty) {
    outputQueue ++= data
    flush()
  }
  
  def recv[A](decoder: Decoder[A]): Future[A] = {
    val result = Promise[A]()
    inputQueue pushDecoder { decoder map { result success _ } }
    result
  }

  def send[A](data: A, encoder: Encoder[A]) : Unit = {
    encoder(outputQueue, data)
    flush()
  }

  def recv(receiver: ActorRef, decoder: Decoder[_], repeat: Boolean): Unit = inputQueue pushDecoder {
    val action = decoder map { receiver ! _ }
    if (repeat) IO.repeat { action }
    else action
  }
  
  override def close(): Unit = {
    if (isOpenOpt.isEmpty) setIsOpen(false)
    super.close()
  }
}
