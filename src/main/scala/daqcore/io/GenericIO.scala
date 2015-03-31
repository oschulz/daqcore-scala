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

import akka.actor.{ActorRef, ActorRefFactory}
import scala.concurrent.Future
import akka.util.{Timeout}

import daqcore.util._
import daqcore.actors._


trait GenericInput[+A] extends Closeable {
  def recv(): Future[A]

  def recv(receiver: ActorRef, repeat: Boolean): Unit
}


trait GenericOutput[-A] extends Closeable with Syncable {
  def send(data: A) : Unit

  def flush() : Unit
}


trait GenericIO[A] extends GenericInput[A] with GenericOutput[A]



trait ByteStreamInput extends GenericInput[ByteString] {
  def recv[A](decoder: Decoder[A]): Future[A]

  def recv(receiver: ActorRef, decoder: Decoder[_], repeat: Boolean): Unit
}

trait ByteStreamOutput extends GenericOutput[ByteString] {
   def send[A](data: A, encoder: Encoder[A]) : Unit
}

trait ByteStreamIO extends GenericIO[ByteString] with ByteStreamInput with ByteStreamOutput

object ByteStreamIO extends IOResourceCompanion[ByteStreamIO] {
  def newInstance =
    RawMsgIO.newInstance orElse
    InetConnection.newInstance orElse
    VICPLink.newInstance
}


trait RawMsgInput extends ByteStreamInput

trait RawMsgOutput extends ByteStreamOutput

trait RawMsgIO extends ByteStreamIO with RawMsgInput with RawMsgOutput

object RawMsgIO extends IOResourceCompanion[RawMsgIO] {
  def newInstance = VXI11Link.newInstance
}


trait ByteFrameInput extends GenericInput[ByteString] {
  def recv[A](decoder: Decoder[A]): Future[A]

  def recv(receiver: ActorRef, decoder: Decoder[_], repeat: Boolean): Unit
}

trait ByteFrameOutput extends GenericOutput[ByteString] {
   def send[A](data: A, encoder: Encoder[A]) : Unit
}

trait ByteFrameIO extends GenericIO[ByteString] with ByteFrameInput with ByteFrameOutput

object ByteFrameIO extends IOResourceCompanion[ByteFrameIO] {
  def newInstance =
    UDPClient.newInstance
}
