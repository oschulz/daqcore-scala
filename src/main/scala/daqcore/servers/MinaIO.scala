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


package daqcore.servers

import scala.actors._
import java.io.IOException

import org.apache.mina.core.buffer.IoBuffer
import org.apache.mina.core.service.{IoHandler, IoHandlerAdapter}
import org.apache.mina.core.session.{IoSession, IdleStatus}
import org.apache.mina.core.service.IoHandlerAdapter
import org.apache.mina.transport.socket.nio.NioSocketConnector
import org.apache.mina.core.future.{IoFutureListener, ConnectFuture}

import daqcore.util._
import daqcore.actors._
import daqcore.profiles._


trait MinaIO {
  class MinaConnection(val session: IoSession) extends Server {
    def defaultTimeout: Long = 60000

    val profiles = Set(profileOf[StreamIO], profileOf[StreamReader], profileOf[StreamWriter], profileOf[Closeable])
    
    protected[MinaIO] object readQueue extends DaemonActor with Logging {
      import scala.actors.Actor._
      def act() = loop { react {
        case StreamIO.Read(timeout) => {
          val replyTo = sender
          reactWithin (if (timeout < 0) defaultTimeout else timeout) {
            case InputData(bytes) => replyTo ! bytes
          }
        }
        case Closed => {
          trace("Closed")
          exit('Closed)
        }
      } }
    }

    protected object writeQueue extends DaemonActor with Logging  {
      import scala.actors.Actor._
      def act() = loop { react {
        case StreamIO.Write(data) => {
          val buffer = IoBuffer.wrap(data.toArray)
          session.write(buffer)
        }
        case _ => throw new RuntimeException("unknown message")
      } }
    }
    
    override def init() = {
      link(readQueue); readQueue.start()
      link(writeQueue); writeQueue.start()
    }
    
    def serve = {
      case r: StreamIO.Read => readQueue.forward(r)
      case w: StreamIO.Write => writeQueue.forward(w)
      case Closeable.Close => {
        session.close(true)
        exit('Closed)
      }
    }
  }

  
  protected class ConnectionHandler extends IoHandlerAdapter with Logging {
    var sessions = Map.empty[IoSession, MinaConnection]
    
    def getServer(session: IoSession) = sessions(session)
    
    override def sessionOpened(session: IoSession) = {
      trace("sessionOpened(%s)".format(session))
    }

    override def sessionClosed(session: IoSession) = {
      trace("sessionClosed(%s)".format(session))
      val server = getServer(session)
      sessions -= session
      server.readQueue ! Closed
    }

    // def override sessionIdle(session: IoSession, status: IdleStatus) = {}

    override def messageReceived(session: IoSession, message: AnyRef) = {
      trace("messageReceived(%s, %s)".format(session, message))
      val server = getServer(session)
      val buffer = message.asInstanceOf[IoBuffer]
      // while (buf.hasRemaining()) { ... buf.get ... }
      assert(buffer.hasArray)
      server.readQueue ! InputData(ByteCharSeq(buffer.array))
    }
  }


  case class InputData(bytes: ByteCharSeq)

  object Closed
}



class MinaConnector extends Server with MinaIO {
  protected val profiles = Set(profileOf[InetConnector], profileOf[Closeable])
  
  protected val defaultTimeout: Long = 30000

  protected var handler: ConnectionHandler = null
  protected var connector: NioSocketConnector = null
  
  override def init() = {
    handler = new ConnectionHandler
    connector = new NioSocketConnector
    connector.setConnectTimeoutMillis(defaultTimeout)
    connector.setHandler(handler)
  }

  def serve = {
    case Closeable.Close => exit()
    case InetConnector.Connect(to, timeout) => {
      val replyTo = sender
      
      connector.setConnectTimeoutMillis (
        if (timeout < 0) defaultTimeout else timeout
      )
      val cf = connector.connect(to)
      cf.addListener (
        new IoFutureListener[ConnectFuture] {
          // Called _before_ ConnectionHandler.sessionOpened:
          def operationComplete(future: ConnectFuture) = {
            val isConnected = future.isConnected
            trace("ConnectFutureListener: isConnected: %s".format(isConnected))
            if (isConnected) {
              val session = future.getSession
              val server = new MinaConnection(session)
              server.start
              handler.sessions += session -> server
              replyTo ! server
              trace("IoFutureListener got session %s".format(session))
            } else {
              throw new IOException("MinaConnector: Can't establish connection")
              debug("can't connect")
            }
            
          }
        }
      )
      connector.setConnectTimeoutMillis(defaultTimeout)
    }
      
  }

  override def deinit() = {
    import scala.collection.JavaConversions._
    // close all sessions immediately
    connector.getManagedSessions foreach { _._2.close(true) }
    // dispose should not block now
    connector.dispose()
    connector = null
    handler = null
  }
}
