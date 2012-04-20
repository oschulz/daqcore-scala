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

import akka.actor.ActorRef
import akka.dispatch.Future
import akka.util.{ByteString, Timeout}

import daqcore.actors._


trait GenericInput[A] {
  def recv(): Future[A]

  def recvOnce(receiver: ActorRef): Unit
  def recvAll(receiver: ActorRef): Unit
}


trait GenericOutput[A] extends Syncable {
  def send(data: A) : Unit

  def flush() : Unit
}


trait GenericIO[A] extends GenericInput[A] with GenericOutput[A]



trait ByteStreamInput extends GenericInput[ByteString] {
  def recv[A](decoder: Decoder[A]): Future[A]

  def recvOnce(receiver: ActorRef, decoder: Decoder[_]): Unit

  def recvAll(receiver: ActorRef, decoder: Decoder[_]): Unit
}

trait ByteStreamOutput extends GenericOutput[ByteString] {
   def send[A](data: A, encoder: Encoder[A]) : Unit
}

trait ByteStreamIO extends GenericIO[ByteString] with ByteStreamInput with ByteStreamOutput



trait RawMsgInput extends ByteStreamInput

trait RawMsgOutput extends ByteStreamOutput

trait RawMsgIO extends ByteStreamIO with RawMsgInput with RawMsgOutput
