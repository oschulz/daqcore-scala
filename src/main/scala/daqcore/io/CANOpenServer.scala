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
import scala.concurrent.{Future, ExecutionContext}

import daqcore.util._
import daqcore.io.prot.canopen._


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


object CANOpenServer {
  case class VariableIO(server: CANOpenServer, node: Int) {
    implicit class VariableReader[A](variable: COVar[A, COReadable[A]]) {
      def read(implicit ctx: ExecutionContext): Future[A] = server.readObject(node, variable.index, variable.subIndex) map variable.access.decode
    }

    implicit class VariableWriter[A](variable: COVar[A, COWritable[A]]) {
      def write(value: A)(implicit ctx: ExecutionContext): Future[Unit] = server.writeObject(node, variable.index, variable.subIndex, variable.access.encode(value))
    }

    implicit class ArrayReader[A](array: COArray[A, COReadable[A], COSize])(implicit ctx: ExecutionContext) {
      def read: Future[Seq[A]] = for {
		n <- new VariableReader(array.size).read
		r <- Future.sequence( for { i <- 1 to n } yield new VariableReader(array(i)).read )
      } yield r
    }

    implicit class RecordReader(record: CORecord)(implicit ctx: ExecutionContext) {
      def read: Future[Seq[(String, Any)]] = Future.sequence (
        for {
          m <- record.members
          if (m.isReadable)
        } yield {
          val value = new VariableReader(m.asInstanceOf[COReadableVar[Any]]).read
          value map { v => m.name -> v}
        }
      )
    }
  }
}
