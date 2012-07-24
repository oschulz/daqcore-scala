// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import java.net.{SocketAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorRefFactory}
import akka.dispatch.{Future, Promise}

import daqcore.util._


trait ForkedForeach[A] extends Closeable {
  def pause(): Unit
  def isPaused: Future[Boolean]

  def cont(): Unit
  def stop(): Unit
  
  def pending: Future[Seq[A]]
}


object ForkedForeach {
  protected case object DoNext

  def apply[A](input: Seq[A], start: Boolean = true, name: String = "")(body: A => Unit)(onDone: => Unit = {})(implicit rf: ActorRefFactory): ForkedForeach[A] =
    typedActorOf[ForkedForeach[A]](new ForeachImpl(input, body, start, onDone), name)


  class ForeachImpl[A](input: Seq[A], body: A => Unit, start: Boolean, onDone: => Unit) extends ForkedForeach[A] with TypedActorImpl with CloseableTAImpl {
    var iterator = input.iterator
    var paused = !start
    var doNextPending = false

    if (!paused) cont()
    
    def pause() { paused = true }
    
    def isPaused() = successful(paused)

    def cont() {
      paused = false
      if (!doNextPending) {
        selfRef ! ForkedForeach.DoNext
        doNextPending = true
      }
    }
    def stop { close() }

    def pending = {
      val (a, b) = iterator.duplicate
      iterator = a
      successful(b.toSeq)
    }

    override def receive = extend(super.receive) {
      case ForkedForeach.DoNext => {
        doNextPending = false
        if (iterator.hasNext) {
          if (!paused) {
            body(iterator.next())
            cont()
          }
        } else {
          onDone
          close()
        }
      }
    }
  }
}
