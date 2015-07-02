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
import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future, Promise, ExecutionContext}

import daqcore.util._


trait ForkedForeach[A] extends Closeable {
  def pause(): Unit
  def isPaused: Future[Boolean]
  def current: Future[Option[A]]
  
  def cont(): Unit
  def stop(): Unit
  
  def pending: Future[Seq[A]]
}


object ForkedForeach {
  protected case object DoNext
  protected case class ElemDone(result: Try[_])

  def apply[A, B](input: Seq[A], start: Boolean = true, name: String = "")(body: A => B)(onDone: => Unit = {})(implicit rf: ActorRefFactory): ForkedForeach[A] =
    typedActorOf[ForkedForeach[A]](new ForeachImpl(input, body, start, onDone), name)


  class ForeachImpl[A, B](input: Seq[A], body: A => B, start: Boolean, onDone: => Unit) extends ForkedForeach[A] with TypedActorImpl with CloseableTAImpl {
    import ForeachImpl._

    protected var pendingInput = input
    protected var paused = !start
    protected var currentElem: Option[A] = None
    protected var doNextPending = false
    protected var elemDonePending = false

    protected def doNext() = if (!doNextPending) selfRef ! DoNext
    
    if (!paused) cont()
    
    def pause() { paused = true }
    
    def isPaused() = successful(paused)

    def current() = successful(currentElem)

    def cont() {
      paused = false
      if (!elemDonePending) doNext()
    }

    def stop {
      onDone
      close()
    }

    def pending = successful(pendingInput)

    override def receive = extend(super.receive) {
      case DoNext => {
        doNextPending = false
        if (!pendingInput.isEmpty) {
          if (!paused) {
            val elem = pendingInput.head
            pendingInput = pendingInput.tail
            currentElem = Some(elem)
            log.trace("Processing next element: " + loggable(elem))
            val result = body(elem)
            log.trace("Immediate result: " + loggable(result))
            result match {
              case future: Future[_] =>
                elemDonePending = true
                ForeachImpl.
                sendElemDone(selfRef, future)(defaultExecContext)
              case r => doNext()
            }
          } else {
            currentElem = None
          }
        } else {
          onDone
          close()
        }
      }

      case ElemDone(result) => {
        elemDonePending = false
        log.trace("Future element result: " + loggable(result))
        result match {
          case Failure(t) => throw t
          case Success(_) =>
            if (!paused) doNext()
            else currentElem = None
        }
      }
    }
  }

  object ForeachImpl {
    protected def sendElemDone(actorRef: ActorRef, future: Future[_])(implicit executor: ExecutionContext) =
      future onComplete { r => actorRef ! ElemDone(r) }
  }
}
