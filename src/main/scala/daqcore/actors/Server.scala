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


package daqcore.actors

import akka.actor._

import daqcore.util._


trait ServerInterface {
  def srv: ActorRef
  def stop()
}


trait ServerProfile extends ServerInterface {
  def defaultTimeout: Long = ActorRefOps.defaultTimeout
  
  def stop() { srv.stop }
  def close() { stop() }

  lazy val profiles: ProfileSet = srv !> Server.GetProfiles

  protected def supports(cl: Class[_]) = profiles covers cl

  protected def requireProfile(cl: Class[_]): Unit =
    if (!supports(cl)) throw new IllegalArgumentException("Server does not support profile " + cl)

  for { cl <- this.getClass.getInterfaces; if (ProfileSet.isProfile(cl)) } { requireProfile(cl) }
}

class ServerProxy(val srv: ActorRef) extends ServerProfile


trait Server extends Actor with Logging with Profiling {
  import scala.actors.Actor._
  import Server._
  
  final def srv: ActorRef = self
  def defaultTimeout: Long = ActorRefOps.defaultTimeout
  
  def profiles = ProfileSet(classOf[ServerProfile])
 
  def replyTarget: MsgTarget = self.channel
  
  def reply(msg: Any): Unit = self.reply(msg)
  
  protected[actors] var restarted = false


  val instanceUuid = newUuid()
  log.trace("New instance of %s, uuid %s, instance %s".format(this.getClass, self.uuid, instanceUuid))

  protected[actors] var clientsLinked = Set[(ActorRef, Uuid)]()
  protected[actors] var serversLinked = Set[ActorRef]()


  /** Servers may override this */
  def init(): Unit = {
    withCleanup
      { log.debug("Server %s initializing".format(srv)) }
      { log.debug("Server %s cleaned up".format(srv)) }
  }
  

  /** Servers may override this */
  def onServerExit(server: ActorRef, reason: Option[Throwable]): Unit = reason match {
    case Some(exception) =>
      throw new RuntimeException("Linked server %s crashed with %s".format(server, exception))
    case None =>
      throw new RuntimeException("Linked server %s shut down unexpectedly".format(server))
  }

  
  protected[actors] def runCleanupActions(): Unit = {
    while (cleanupActions != Nil) {
      val action::rest = cleanupActions
      cleanupActions = rest
      try { action() } catch { case e => log.error(e.toString) }
    }
  }

  protected[actors] def runShutdownActions(): Unit = {
    while (shutdownActions != Nil) {
      val action::rest = shutdownActions
      shutdownActions = rest
      try { action() } catch { case e => log.error(e.toString) }
    }
  }


  /** Servers must implement this */
  def serve: PartialFunction[Any, Unit]


  def initOnce(body: => Unit): Unit = { if (!self.isBeingRestarted) body }

  protected[actors] var cleanupActions: List[() => Unit] = Nil

  protected[actors] var shutdownActions: List[() => Unit] = Nil

  def atCleanup(body: => Unit): Unit = { cleanupActions = { (() => body) :: cleanupActions } }

  def atShutdown(body: => Unit): Unit = { shutdownActions = { (() => body) :: shutdownActions } }
  
  def withCleanup (initBody: => Unit)(cleanupBody: => Unit) {
    atCleanup { cleanupBody }
    initBody
  }

  type SupportsClose = { def close(): Unit }

  def closeAtCleanup[T <: SupportsClose](res: T): T = {
    atCleanup { () => res.close() }
    res
  }


  // client links are not restart-pesistant
  final def clientLinkTo(server: ActorRef) = {
    log.trace("Client-linking to %s".format(server))
    server.!>(AddClientLink(self, instanceUuid))
    serversLinked = serversLinked + server
  }

  final def clientUnlinkFrom(server: ActorRef) = {
    log.trace("Client-unlinking from %s".format(server))
    try { server.!(RemoveClientLink(self, instanceUuid)) }
    catch { case _ => }
    serversLinked = serversLinked - server
  }

  protected[actors] def processClientLinks(reason: Option[Throwable]) {
    for { server <- serversLinked } clientUnlinkFrom(server)
    serversLinked = Set[ActorRef]()
    for { (actor, uuid) <- clientsLinked }
      { try { actor ! ServerExit(self, reason) } catch { case _ => } }
    clientsLinked = Set[(ActorRef, Uuid)]()
  }


  protected[actors] def handleDefaults: PartialFunction[Any, Unit] = {
    case op @ GetProfiles => {
      log.trace(op.toString)
      reply(profiles)
    }
    
    case op @ AddClientLink(client, instance) => {
      log.trace(op.toString)
      clientsLinked = clientsLinked + ((client, instance))
      reply()
    }
    
    case op @ RemoveClientLink(client, instance) => {
      log.trace(op.toString)
      try { clientsLinked = clientsLinked - ((client, instance)) }
      catch { case _ => }
    }
    
    case op @ ServerExit(server, reason) => {
      log.trace(op.toString)
      onServerExit(server, reason)
    }
  }


  protected[actors] val defaultReceive = {
    val recv = handleDefaults orElse serve
    if (Server.timing) profilingTimer("EventLoop") wrap { recv }
    else recv
  }

  def receive = defaultReceive

  override def preStart = {
    log.debug("preStart")
    init()
  }

  override def preRestart(reason: Throwable) {
    log.debug("preRestart(%s)".format(reason))

    try {
      runCleanupActions()
    } finally {
      processClientLinks(Some(reason))
    }
  }

  override def postRestart(reason: Throwable) {
    log.debug("postRestart(%s)".format(reason))
  }
  
  override def postStop = {
    log.debug("postStop")
    
    try {
      runCleanupActions()
      runShutdownActions()
    } finally {
      processClientLinks(None)
      // Stop linked actors
      val laIt = self.linkedActors.values.iterator
      while (laIt.hasNext) {
        val a = laIt.next
        try { a.stop(); self.unlink(a) }
        catch { case e => log.error("Failed to stop linked actor %s: %s".format(a, e)) }
      }
    }
  }
}


object Server {
  case object GetProfiles extends ActorQuery[ProfileSet]
  case class AddClientLink(client: ActorRef, instance: Uuid) extends ActorQuery[Unit]
  case class RemoveClientLink(client: ActorRef, instance: Uuid) extends ActorCmd
  case class ServerExit(server: ActorRef, reason: Option[Throwable]) extends ActorCmd
  
  @volatile var timing: Boolean =  akka.config.Config.config.getBool("daqcore.server.timing", false)
}
