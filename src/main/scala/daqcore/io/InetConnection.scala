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

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}
import akka.actor._
import akka.pattern.{ ask, pipe }

import daqcore.util._
import daqcore.actors._, daqcore.actors.TypedActorTraits._


trait InetConnection extends ByteStreamIO with CloseableTA {
}


object InetConnection extends IOResourceCompanion[InetConnection] {
  def newInstance = {
    case HostURL("tcp", host, Some(port)) =>
      () => new ClientConnectionImpl(new InetSocketAddress(host, port))
  }

  def apply(address: InetSocketAddress)(implicit rf: ActorRefFactory): InetConnection =
    typedActorOf[InetConnection](new ClientConnectionImpl(address))

  def apply(host: String, port: Int)(implicit rf: ActorRefFactory): InetConnection =
    apply(new InetSocketAddress(host, port))


  abstract class ConnectionImpl extends InetConnection with ByteStreamIOImpl {
    import akka.io.{ IO, Tcp }

    def connection: Future[ActorRef]
    // This actor, since it has been registered with the connection, will be
    // monitored by it. Therefore, no Tcp.Closed has to be sent explicitely
    // when this actor stops.

    def connectionOpt = connection.value match {
      case Some(Success(v)) => Some(v)
      case Some(Failure(exception)) => throw exception
      case None => None
    }

    protected def connectionRef = connectionOpt.getOrElse(throw new RuntimeException("TCP Connection not open (yet)"))

    override def isOpen(): Future[Boolean] = connection map { _ => true }

    def flush(): Unit = {
      val bytes = outputQueue.result
      if (! bytes.isEmpty) {
        connectionRef ! Tcp.Write(bytes)
        outputQueue.clear
      }
    }

    override def receive = extend(super.receive) {
      case Tcp.Received(bytes) => {
        log.trace("Received: " + loggable(bytes))
        inputQueue pushData bytes
      }
      case closedMsg: Tcp.ConnectionClosed => {
        closedMsg match {
          case Tcp.Aborted => log.warn("Connection aborted")
          case Tcp.Closed => log.debug("Connection closed normally")
          case Tcp.ConfirmedClosed => log.trace("Connection confirmed closed")
          case Tcp.ErrorClosed(cause) => log.error("Connection closed due to error: " + cause)
          case Tcp.PeerClosed => log.debug("Connection closed by peer")
        }

        log.trace("Connection closed")
        close()
      }
      case Tcp.CommandFailed(cmd) => throw new RuntimeException("TCP Command failed: " + cmd)
    }
  }


  class ClientConnectionImpl(address: InetSocketAddress) extends ConnectionImpl with TypedActorImpl {
    trait Connector {
      def connect(address: InetSocketAddress): Future[ActorRef]
    }

    class ConnectorImpl(handler: ActorRef, logging: Logging) extends Connector with TypedActorBasics with TypedActorReceive {
      import akka.io.{IO, Tcp}

      val connPromise = Promise[ActorRef]()

      def connect(address: InetSocketAddress): Future[ActorRef] = {
        IO(Tcp)(context.system) ! Tcp.Connect(address)
        connPromise.future
      }

      override def receive = extend(super.receive) {
        case Tcp.Connected(remote, local) ⇒
          logging.log.debug("Established connection from " + local + " to " + remote)
          val connection = TypedActor.context.sender
          connPromise success connection
          connection ! Tcp.Register(handler)
          selfStop()

        case Tcp.CommandFailed(_: Tcp.Connect) => throw new RuntimeException("Failed to connect")
      }
    }

    import daqcore.defaults.defaultTimeout
    val connector = typedActorOf[Connector](new ConnectorImpl(selfRef, this), "connector")
    val connection = connector.connect(address);

    connection.get // Wait for connection
  }
}



trait InetServer extends CloseableTA {
}


object InetServer {
  import InetConnection.{ConnectionImpl}


  def apply(address: InetSocketAddress)(body: InetConnection => Unit)(implicit rf: ActorRefFactory): InetServer =
    typedActorOf[InetServer](new ServerImpl(address, body))

  def apply(address: InetSocketAddress, name: String)(body: InetConnection => Unit)(implicit rf: ActorRefFactory): InetServer =
    typedActorOf[InetServer](new ServerImpl(address, body), name)

  def apply(port: Int)(body: InetConnection => Unit)(implicit rf: ActorRefFactory): InetServer =
    apply(new InetSocketAddress(port))(body)

  def apply(port: Int, name: String)(body: InetConnection => Unit)(implicit rf: ActorRefFactory): InetServer =
    apply(new InetSocketAddress(port), name)(body)


  class ServerConnectionImpl(acceptedConn: ActorRef) extends ConnectionImpl with TypedActorImpl {
    val connectionPromise = Promise[ActorRef]()
    val connection = connectionPromise.future
    connectionPromise success acceptedConn
  }


  class ServerImpl(val address: InetSocketAddress, body: InetConnection => Unit) extends InetServer with TypedActorImpl with CloseableTAImpl with Supervisor {
    import akka.io.{IO, Tcp}

    IO(Tcp)(context.system) ! Tcp.Bind(selfRef, address)

    def supervisorStrategy = OneForOneStrategy() { case _ ⇒ SupervisorStrategy.Stop }

    override def receive = extend(super.receive) {
      case Tcp.Bound(localAddress) => log.debug("TCP Server is listening on " + localAddress)

      case Tcp.Connected(remote, local) => {
        log.debug("Accepted connection from " + remote + " to " + local)
        val connection = TypedActor.context.sender
        val handler = typedActorOf[InetConnection](new ServerConnectionImpl(connection))
        connection ! Tcp.Register(actorRef(handler))
      }

      case Tcp.CommandFailed(cmd: Tcp.Bind) => throw new RuntimeException("TCP Command failed " + cmd)
    }
  }
}
