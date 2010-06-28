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


import scala.actors._, scala.actors.Actor._

import daqcore.util._


trait ServerAccess extends Logging {
  def srv: AbstractActor

  val profiles: Set[ProfileInfo]

  protected def supports(profile: ProfileInfo) = profiles.contains(profile)
  
  def requireProfile(p: ProfileInfo): Unit =
    if (!supports(p)) throw new IllegalArgumentException("Proxy target actor does not support profile " + p)

  def as[T](body: => Any) = (body).asInstanceOf[T]
}



trait Profile extends ServerAccess



trait Server extends ServerAccess with DaemonActor with Profile {
  import Server._

  type ReplyTarget = OutputChannel[Any]
  
  def srv: AbstractActor = this
  
  def replyTarget: ReplyTarget = sender
  
  val profiles: Set[ProfileInfo] =
    ProfileInfo.profilesOf(this.getClass)

  private[actors] var restarted = false

  /** Servers may override this */
  protected def onStart(): Unit =
    { debug("Server %s started".format(srv)) }

  /** Servers may override this */
  protected def onRestart(): Unit =
    { debug("Server %s restarted".format(srv)) }

  /** Servers may override this */
  protected def init(): Unit = {
    withCleanup
      { trace("Server %s initializing".format(srv)) }
      { trace("Server %s cleaned up".format(srv)) }
  }

  /** Servers must implement this */
  protected def serve: PartialFunction[Any, Unit]

  /** Servers may override this */
  protected def onKill(reason: AnyRef): Unit =
    { debug("Server %s killed: %s".format(srv, reason)) }

  //** Servers may override this */
  protected def onShutdown(): Unit =
    { debug("Server %s shut down".format(srv)) }
  
  protected[actors] def handleGenericPre: PartialFunction[Any, Unit] = {
    case GetProfiles => reply(profiles)
  }

  protected[actors] def handleGenericPost: PartialFunction[Any, Unit] = {
    case x => throw new RuntimeException("unknown message: " + x.asInstanceOf[AnyRef].getClass.toString)
  }

  def act() = {
    exitMonitor.startOrRestart()
    exitMonitor !? 'ready
    
    if (!restarted) { restarted = true; onStart() } else onRestart()
    init()
    
    eventloop (
      handleGenericPre orElse
      serve orElse
      handleGenericPost
    )
  }


  protected[actors] var cleanupActions: List[() => Unit] = Nil
  
  def withCleanup (initBody: => Unit)(cleanupBody: => Unit) {
    cleanupActions = {() => cleanupBody} :: cleanupActions
    initBody
  }

  protected[actors] def postExit(reason: AnyRef) {
    for (action <- cleanupActions) try { action() } catch { case e => error(e) }
    cleanupActions = Nil

    reason match {
      case 'normal | 'closed => try { onShutdown() } catch { case e => error(e) }
      case reason => try { onKill(reason) } catch { case e => error(e) }
    }
  }

  lazy val exitMonitor = new ExitMonitor
  
  protected class ExitMonitor extends DaemonActor with Logging {
    trapExit = true

    def act = {
      link(srv)
      ready()
    }

    protected def ready() = react {
      case 'ready => {
        reply()
        waitForExit()
      }
      case _ => throw new RuntimeException("ExitMonitor: unexpexted message")
    }
    
    protected def waitForExit() = react {
      case Exit(from, reason) if (from == srv) =>
        postExit(reason)
      case _ => throw new RuntimeException("ExitMonitor: unexpexted message")
    }
  }
}


object Server {
  case object GetProfiles
}



class ServerProxy(val srv: AbstractActor) extends ServerAccess with OutputChannel[Any] with CanReply[Any,Any] {
  type Future[+P] = srv.Future[P]

  lazy val profiles = as[Set[ProfileInfo]] (srv !? Server.GetProfiles)
  
  def !(msg: Any) = srv.!(msg)
  def !?(msec: Long, msg: Any) = srv.!?(msec, msg)
  def !?(msg: Any) = srv.!?(msg)
  def !![A](msg: Any, handler: PartialFunction[Any, A]) = srv.!!(msg, handler)
  def !!(msg: Any) = srv.!!(msg)
  
  def forward(msg: Any) = srv.forward(msg)
  def receiver = srv.receiver
  def send(msg: Any, replyTo: OutputChannel[Any]) = srv.send(msg, replyTo)

  def linkTo() = Actor.link(srv)
  def unlinkFrom() = Actor.unlink(srv)

  requireProfile(ProfileInfo.apply[Server])

  ProfileInfo.profilesOf(this.getClass) foreach {requireProfile(_)}
}
