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

import scala.reflect.ClassManifest
import scala.collection.mutable.{Queue => MQueue}

import java.io.IOException
import akka.actor._, akka.actor.Actor._, akka.dispatch.Future
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle, Temporary, OneForOneStrategy, AllForOneStrategy}

import daqcore.util._
import daqcore.actors._
import daqcore.io.prot.rootsys._


class RootSystemServer() extends CascadableServer with KeepAlive with PostInit {
  import RootSystemServer._

  case class ActiveQuery(id: Int, readResp: BasicInput => Any, replyTo: MsgTarget)
  
  var io: RawMsgIO = _
  
  var msgId = 0
  var queries: MQueue[ActiveQuery] = null
  implicit var serCache: ContentSerCache = null
  
  override def init() = {
    super.init()
    
    queries = MQueue[ActiveQuery]()
    serCache = ContentSerCache()
  }

  override def postInit() = {
    super.postInit()
    
    io = RootSystemProcess(srv, Temporary)
    io.triggerRecv()
    io.triggerRecv()
    atShutdown { io.close() }
  }

  def sendRequest(request: RootSysRequest): Int = {
    msgId += 1
    val msgBuffer = RootEncOutput(ByteSeqBuilder(1024))
    msgBuffer.writeInt(requestMsgType)
    msgBuffer.writeInt(msgId)
    request.writeRequest(msgBuffer)
    io.send(msgBuffer.result())
    io.flush()
    msgId
  }
  
  def handleRootSysMsg(msg: ByteSeq): Unit = {
    io.triggerRecv()
    val ActiveQuery(id, readResp, replyTo) = queries.dequeue()
    val source = RootEncInput(msg.iterator)

    val msgType = source.readInt()
    if (msgType != responseMsgType) throw new IOException("Unknown message type " + msgType)
    val msgId = source.readInt()
    if (msgId != id) throw new IOException("Unexpected message id " + msgId)
    
    val resp = readResp(source)
    replyTo ! resp
  }
  
  def srvCmd(cmd: RootSysCmd): Unit = {
    sendRequest(cmd)
  }
  
  def srvQuery(query: RootSysQuery[_]): Unit = {
    val id = sendRequest(query)
    queries.enqueue(ActiveQuery(id, query.readResponse, replyTarget))
  }
  
  override def serve = super.serve orElse {
    case op: RootSysCmd => srvCmd(op)
    case op: RootSysQuery[_] => srvQuery(op)
    case recv @ RawMsgInput.Received(msg) => handleRootSysMsg(msg)
  }
}


object RootSystemServer extends {
  val requestMsgType = 0x52455155
  val responseMsgType = 0x52455350

  def apply(sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): ServerProfile = {
    new ServerProxy(sv.linkStart(actorOf(new RootSystemServer()), lc))
  }
}
