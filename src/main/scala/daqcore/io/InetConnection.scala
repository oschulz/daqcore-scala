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
   def recv(): Future[ByteString]
   def send(data: ByteString) : Unit
   def close(): Unit
   def flush(): Unit
}


object InetConnection {
  def apply(address: InetSocketAddress): InetConnection =
    typedActorOf[InetConnection](new InetConnectionImpl(address))
  
  def apply(host: String, port: Int): InetConnection =
    apply(new InetSocketAddress(host, port))
}


class InetConnectionImpl(address: InetSocketAddress) extends InetConnection with TypedActorImpl with CloseableImpl with MsgReceive  {
  import akka.actor.{IO, IOManager}

  val inputQueue = new DataActionQueue[ByteString]

  var socket: IO.SocketHandle = IOManager(actorSystem).connect(address)(selfRef)
  atCleanup { socket.close() }

  def recv(): Future[ByteString] = {
    val result = Promise[ByteString]()
    inputQueue pushAction { result success _ }
    result
  }
  
  def send(data: ByteString) : Unit = socket.write(data)
  
  def flush(): Unit = {} // Automatic flush on every send
  
  def msgReceive = {
    case (IO.Connected(socket, address), _) => {
      println("Established connection to " + address)
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
