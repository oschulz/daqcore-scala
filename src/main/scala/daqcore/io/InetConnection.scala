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

import daqcore.util._
import daqcore.actors._, daqcore.actors.TypedActorTraits._


trait InetConnection extends ByteStreamIO with CloseableTA {
}


object InetConnection {
  def apply(address: SocketAddress)(implicit rf: ActorRefFactory): InetConnection =
    typedActorOf[InetConnection](new ClientConnectionImpl(address))
  
  def apply(host: String, port: Int)(implicit rf: ActorRefFactory): InetConnection =
    apply(new InetSocketAddress(host, port))


  abstract class ConnectionImpl extends InetConnection with ByteStreamIOImpl {
    protected val socket: IO.SocketHandle
    atCleanup { socket.close() }
    
    def flush(): Unit = {
      val bytes = outputQueue.result
      if (! bytes.isEmpty) {
        socket.write(bytes)
        outputQueue.clear
      }
    }
    
    override def msgReceive = extend(super.msgReceive) {
      case (IO.Connected(socket, address), _) => {
        setIsOpen(true)
        log.trace("Established connection to " + address)
      }
      case (IO.Read(socket, bytes), _) => {
        log.trace("Received: " + loggable(bytes))
        inputQueue pushData bytes
      }
      case (IO.Closed(socket: IO.SocketHandle, cause), _) => {
        log.trace("Connection closed because: " + cause)
        close()
      }
    }
  }


  class ClientConnectionImpl(address: SocketAddress) extends ConnectionImpl with TypedActorImpl with CloseableTAImpl with MsgReceive {
    val socket: IO.SocketHandle = IOManager(actorSystem).connect(address)(selfRef)
  }
}



trait InetServer extends CloseableTA {
}


object InetServer {
  import InetConnection.{ConnectionImpl}

  
  def apply(address: SocketAddress)(body: InetConnection => Unit)(implicit rf: ActorRefFactory): InetServer =
    typedActorOf[InetServer](new ServerImpl(address, body))

  def apply(address: SocketAddress, name: String)(body: InetConnection => Unit)(implicit rf: ActorRefFactory): InetServer =
    typedActorOf[InetServer](new ServerImpl(address, body), name)
    
  def apply(port: Int)(body: InetConnection => Unit)(implicit rf: ActorRefFactory): InetServer =
    apply(new InetSocketAddress(port))(body)

  def apply(port: Int, name: String)(body: InetConnection => Unit)(implicit rf: ActorRefFactory): InetServer =
    apply(new InetSocketAddress(port), name)(body)


  class ServerConnectionImpl(serverHandle: IO.ServerHandle) extends ConnectionImpl with TypedActorImpl with CloseableTAImpl with MsgReceive  {
    val socket: IO.SocketHandle = serverHandle.accept()(selfRef)
  }


  class ServerImpl(val address: SocketAddress, body: InetConnection => Unit) extends InetServer with TypedActorImpl with CloseableTAImpl with MsgReceive with Supervisor {
    val serverHandle = IOManager(actorSystem).listen(address)(selfRef)
    atCleanup { serverHandle.close() }

    def supervisorStrategy = OneForOneStrategy() { case _ ⇒ SupervisorStrategy.Stop }
    
    override def msgReceive = extend(super.msgReceive) {
      case (IO.Listening(server, address), _) => {
        log.trace("Server is listening on " + address)
      }
       
      case (IO.NewClient(server), _) => {
        log.trace("New connection to server")
        val connection = typedActorOf[InetConnection](new ServerConnectionImpl(serverHandle))
        connection.isOpen.getOpt(100 milliseconds) match {
          case Some(true) => connection.isOpen onSuccess { case true => body(connection) }
          case _ => log.trace("Could not open connection - client no longer interested?")
        }
      }
       
      case (IO.Closed(server: IO.ServerHandle, cause), _) => {
        log.trace("Server socket has closed because: " + cause)
      }
    }
  }
}
