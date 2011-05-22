// Copyright (C) 2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import java.io.IOException
import java.net.InetSocketAddress

import akka.actor.Actor.actorOf, akka.actor.{ActorRef}, akka.dispatch.Future
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle, Temporary}
import akka.config.Supervision.{OneForOneStrategy, AllForOneStrategy}

import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}
import org.jboss.netty.channel.{Channel, Channels, ChannelFuture, ChannelHandlerContext, SimpleChannelUpstreamHandler}
import org.jboss.netty.channel.{ChannelStateEvent, ExceptionEvent, MessageEvent}

import daqcore.util._
import daqcore.actors._


object NettyIO {
  abstract class NettyChannelSrv extends CascadableServer {
    var channel: Option[Channel] = None
    
    override def init() = {
      super.init()
      
      atCleanup {
        val chOpt = channel
        channel = None
        for { ch <- chOpt } { val closed = ch.close(); closed.awaitUninterruptibly() }
      }
    }
  }


  object NettyChannelSrv {
    case class ChannelConnected(ch: Channel)

    case class ChannelInputData(bytes: ByteSeq)

    case class ChannelException(e: Throwable)

    case object ChannelClosed
  }


  abstract class NettyConnection extends NettyChannelSrv with QueueingServer {
    import NettyChannelSrv._

    override def profiles = super.profiles.+[InetConnection]

    val recvQueue = new ReplyQueue
    
    override def init() = {
      super.init()
    }

    def onRemoteClose(): Unit = {
        trace("Connection closed remotely")
        self.stop
    }
    
    override def serve = super.serve orElse {
      case ByteStreamInput.Recv() => recvQueue.addTarget(replyTarget)
      case ByteStreamOutput.Send(bytes) => {
        channel.get.write(ChannelBuffers.wrappedBuffer(bytes.toArray))
      }
      case ByteStreamOutput.Flush() => {} // flushes automatically
     
      case msg @ ChannelConnected(ch) => {
        trace(msg)
        channel = Some(ch)
      }
      case msg @ ChannelInputData(bytes) => {
        trace("Received: " + loggable(msg))
        recvQueue.addReply(ByteStreamInput.Received(bytes)){}
      }
      case msg @ ChannelException(e) => {
        warn(msg)
        throw e
      }
      case ChannelClosed => onRemoteClose()
    }
  }


  abstract class NettyConnectionHandler extends SimpleChannelUpstreamHandler with Logging {
    import NettyChannelSrv._

    val aRef: ActorRef

    def closeOnException(ctx: ChannelHandlerContext)(body: => Unit): Unit = {
      try { body }
      catch { case e => ctx.getChannel.close(); log.error(e.toString) }
    }

    override def channelConnected(ctx: ChannelHandlerContext, e: ChannelStateEvent): Unit = {
      log.trace("channelConnected(..., ...)")
      closeOnException(ctx){ aRef ! ChannelConnected(ctx.getChannel) }
    }

    override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent): Unit =  {
      log.trace("messageReceived(..., %s)".format(e.toString))
      val buffer = e.getMessage().asInstanceOf[ChannelBuffer]
      val bytes = Array.ofDim[Byte](buffer.readableBytes)
      buffer.getBytes(0, bytes, 0, bytes.length)
      closeOnException(ctx){ aRef ! ChannelInputData(ByteSeq.wrap(bytes)) }
    }

    override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent): Unit =  {
      val cause = e.getCause()
      log.warn("exceptionCaught(..., %s)".format(cause))
      closeOnException(ctx){ aRef ! ChannelException(cause) }
      e.getChannel().close() // close connection
    }

    override def channelClosed(ctx: ChannelHandlerContext, e: ChannelStateEvent): Unit =  {
      log.trace("channelClosed(..., ...)")
      try { aRef ! ChannelClosed } catch { case _ => }
    }
  }
}


class NettyClientConnection(val address: InetSocketAddress, timeout: Long) extends NettyIO.NettyConnection {
  import NettyIO._

  val nccRef: ActorRef = self

  class NettyClientConnectionHandler extends NettyConnectionHandler { val aRef = nccRef }
  
  override def onRemoteClose(): Unit = {
      trace("Connection closed remotely")
      throw new java.io.IOException("Connection closed by server")
  }

  override def init() = {
    super.init()

    val ch = NettyClientConnection.channelFactory.newChannel(
      Channels.pipeline(new NettyClientConnectionHandler)
    )
    ch.getConfig.setOption("connectTimeoutMillis", timeout)
    ch.getConfig.setOption("keepAlive", true)
    ch.getConfig.setOption("tcpNoDelay", true)
    val connected = ch.connect(address)
    connected.awaitUninterruptibly()
    assert(connected.isDone)
  }
}


object NettyClientConnection {
  import NettyIO._

  import java.util.concurrent.Executors
  import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory
  
  protected val channelFactory = new NioClientSocketChannelFactory(Executors.newCachedThreadPool(), Executors.newCachedThreadPool())

  def apply(to: InetSocketAddress, timeout: Long = 10000, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): ByteStreamIO =
    new ServerProxy(sv.linkStart(actorOf(new NettyClientConnection(to, timeout)), lc)) with ByteStreamIO
}


class NettyServer(val address: InetSocketAddress, body: ByteStreamIO => Unit) extends NettyIO.NettyChannelSrv {
  import NettyIO._

  import org.jboss.netty.bootstrap.ServerBootstrap
  import org.jboss.netty.channel.{ChannelPipeline, ChannelPipelineFactory}

  def supervisor: ActorRef = self
  
  case class NewConnection(aRef: ActorRef) extends ActorCmd
  
  class NettyServerConnection extends NettyConnection {
    override def init() = {
      super.init()
      self.lifeCycle = Temporary
      self.faultHandler = OneForOneStrategy(List(classOf[Throwable]), 3, 1000)
    }
  }
  
  class NettyServerConnectionHandler extends NettyConnectionHandler {
    val aRef = actorOf(new NettyServerConnection)
    supervisor.link(aRef)
    aRef.start()
    supervisor ! NewConnection(aRef)
  }

  override def init() = {
    super.init()

    self.faultHandler = OneForOneStrategy(List(classOf[Throwable]), 3, 1000)

    val bootstrap = new ServerBootstrap(NettyServer.channelFactory)
    bootstrap.setPipelineFactory(
      new ChannelPipelineFactory() {
        def getPipeline() = Channels.pipeline(new NettyServerConnectionHandler)
      }
    )

    channel = Some(bootstrap.bind(address))
  }
  
  override def serve = super.serve orElse {
    case NewConnection(aRef) => body(new ServerProxy(aRef) with ByteStreamIO)
  }
}


object NettyServer {
  import NettyIO._
  import java.util.concurrent.Executors
  import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
  
  protected val channelFactory = new NioServerSocketChannelFactory(Executors.newCachedThreadPool(), Executors.newCachedThreadPool())

  def apply(addr: InetSocketAddress, sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle)(body: ByteStreamIO => Unit): ServerProfile =
    new ServerProxy(sv.linkStart(actorOf(new NettyServer(addr, body)), lc)) with ServerProfile
}
