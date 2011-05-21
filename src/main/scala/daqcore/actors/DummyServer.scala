// Copyright (C) 2011 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import akka.actor.Actor.actorOf, akka.actor.ActorRef, akka.dispatch.Future

import daqcore.actors._


trait DummyInterface extends ServerInterface {
  def identity: Future[String]
  def add(i: Int, j: Int): Future[Int]
  def echo(a: Any): Any
  def log_info(msg: String): Unit
}

object DummyInterface {
  def apply(srv: ActorRef): DummyInterface =
    SReqProxy[DummyInterface](srv)

  def apply(sv: Supervising = defaultSupervisor): DummyInterface =
    DummyInterface(sv.linkStart(new DummyServer()))
}


case class DummyProfile(srv: ActorRef) extends DummyInterface with ServerProfile {
  def identity = srv.qryF[String]('identity)
  def add(i: Int, j: Int) = srv.qryF[Int]('add, i, j)
  def echo(a: Any) = srv.qry[Any]('echo, a)
  def log_info(msg: String) = srv.cmd('log_info, msg)
}

object DummyProfile {
  def apply(sv: Supervising = defaultSupervisor): DummyProfile =
    DummyProfile(sv.linkStart(new DummyServer()))
}


class DummyServer() extends MServer {
  override def profiles = super.profiles.+[DummyProfile]

  @sreq def identity = "DummyServer-" + srv.uuid
  @sreq def add(i: Int, j: Int) = i + j
  @sreq def echo(a: Any) = a
  @sreq def log_info(msg: String) = log.info(msg)
}
