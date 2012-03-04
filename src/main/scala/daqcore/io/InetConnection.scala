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

import java.net.InetSocketAddress

import akka.dispatch.{Future, Promise}
import akka.util.{ByteString, Timeout}

import daqcore.util._
import daqcore.actors._, daqcore.actors.TypedActorTraits._


trait InetConnection extends ByteStreamIO with Closeable {
}


object InetConnection {
  def apply(address: InetSocketAddress): InetConnection =
    typedActorOf[InetConnection](new InetConnectionImpl(address))
  
  def apply(host: String, port: Int): InetConnection =
    apply(new InetSocketAddress(host, port))
}


class InetConnectionImpl(address: InetSocketAddress) extends InetConnection with TypedActorImpl with CloseableImpl with MsgReceive  {
  import akka.actor.{IO, IOManager}

  val inputQueue = new DataDecoderQueue
  val outputQueue = new ByteStringBuilder

  var socket: IO.SocketHandle = IOManager(actorSystem).connect(address)(selfRef)
  atCleanup { socket.close() }

  def recv(): Future[ByteString] = recv(IO takeAny)
  
  def send(data: ByteString) : Unit = if (! data.isEmpty) {
    outputQueue ++= data
    flush()
  }
  
  def flush(): Unit = {
    val bytes = outputQueue.result
    if (! bytes.isEmpty) {
      socket.write(bytes)
      outputQueue.clear
    }
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
  
  def msgReceive = {
    case (IO.Connected(socket, address), _) => {
      trace("Established connection to " + address)
    }
    case (IO.Read(socket, bytes), _) => {
      trace("Received: " + loggable(bytes))
      inputQueue pushData bytes
    }
    case (IO.Closed(socket: IO.SocketHandle, cause), _) => {
      trace("Connection closed because: " + cause)
    }
  }
}
