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


trait UDPClient extends ByteFrameIO with CloseableTA


object UDPClient extends IOResourceCompanion[UDPClient] {
  def newInstance = {
    case HostURL("udp", host, Some(port)) =>
      () => new ClientImpl(new InetSocketAddress(host, port))
  }


  def apply(remoteAddress: InetSocketAddress, localAddress: Option[InetSocketAddress], name: String)(implicit rf: ActorRefFactory): UDPClient =
    typedActorOf[UDPClient](new ClientImpl(remoteAddress, localAddress), name)

  def apply(remoteAddress: InetSocketAddress, name: String)(implicit rf: ActorRefFactory): UDPClient =
    typedActorOf[UDPClient](new ClientImpl(remoteAddress), name)

  def apply(remoteAddress: InetSocketAddress)(implicit rf: ActorRefFactory): UDPClient =
    typedActorOf[UDPClient](new ClientImpl(remoteAddress))

  def apply(host: String, port: Int)(implicit rf: ActorRefFactory): UDPClient =
    apply(new InetSocketAddress(host, port))


  class ClientImpl(remoteAddress: InetSocketAddress, localAddress: Option[InetSocketAddress] = None)
    extends UDPClient with ByteFrameIOImpl
  {
    import akka.io.{ IO, Udp, UdpConnected }
    import daqcore.defaults.defaultTimeout

    var recvActor: Option[ActorRef] = None

    val connection: Future[ActorRef] = {
      IO(UdpConnected)(context.system).askSender(UdpConnected.Connect(selfRef, remoteAddress, localAddress)).map{
        case (UdpConnected.Connected, ref) => ref
      }
    }


    def connectionOpt = connection.value match {
      case Some(Success(v)) => Some(v)
      case Some(Failure(exception)) => throw exception
      case None => None
    }

    protected def connectionRef = connectionOpt.getOrElse(throw new RuntimeException("Connected UDP not ready (yet)"))

    atCleanup { connectionOpt foreach { _ ! UdpConnected.Disconnect } }
    

    def send(data: ByteString) = {
      connectionRef ! UdpConnected.Send(data)
    }

    def flush() = {}


    override def isOpen(): Future[Boolean] = connection map { _ => true }

    override def recv(receiver: ActorRef, repeat: Boolean) = {
      if (repeat) recvActor = Some(receiver)
      else super.recv(receiver, repeat)
    }

    override def receive = extend(super.receive) {
      case UdpConnected.Received(frame) => {
        log.trace("Received: " + loggable(frame))
        recvActor match {
          case Some(actor) => actor ! frame
          case None => inputQueue pushData frame
        }
      }
      case UdpConnected.Disconnected => {
        log.warn("UDP disconnected")
        close()
      }
    }

    connection.get // Wait for connection
  }
}
