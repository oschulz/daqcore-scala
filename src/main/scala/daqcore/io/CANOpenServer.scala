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


package daqcore.io

import akka.actor._
import akka.dispatch.{Future, Promise}

import daqcore.util._


trait CANOpenServer {
  def readObject(node: Int, idx: Int, subIdx: Int): Future[ByteString]
  def readByteObject(node: Int, idx: Int, subIdx: Int): Future[Byte]
  def readShortObject(node: Int, idx: Int, subIdx: Int): Future[Short]
  def readIntObject(node: Int, idx: Int, subIdx: Int): Future[Int]

  def writeObject(node: Int, idx: Int, subIdx: Int, value: ByteString): Future[Unit]
  def writeByteObject(node: Int, idx: Int, subIdx: Int, value: Byte): Future[Unit]
  def writeShortObject(node: Int, idx: Int, subIdx: Int, value: Short): Future[Unit]
  def writeIntObject(node: Int, idx: Int, subIdx: Int, value: Int): Future[Unit]
}
