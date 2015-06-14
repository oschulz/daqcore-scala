// Copyright (C) 2010-2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import java.io.{InputStream, OutputStream, File}

import scala.collection.JavaConversions._
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._

import akka.actor._

import daqcore.util._
import daqcore.actors._


trait ProcessIOStream extends ByteStreamIO with CloseableTA


object ProcessIOStream {
  def apply(args: Seq[String], name: String)(implicit rf: ActorRefFactory): ProcessIOStream =
    typedActorOf[ProcessIOStream](new ProcessIOStreamImpl(args), name)

  def apply(args: String, name: String)(implicit rf: ActorRefFactory): ProcessIOStream =
    apply(Seq(args), name)


  class ProcessIOStreamImpl(args: Seq[String]) extends ProcessIOStream with CloseableTAImpl with SyncableImpl {
    import ProcessIOStreamImpl._

    val pb = new ProcessBuilder(args: _*)
    log.debug("Starting new process: " + pb.command.mkString(" "))
    val process = pb.start

    val stdinWriter = OutputStreamWriter(process.getOutputStream, "stdinWriter")
    atCleanup {
      // stdinWriter should be closed before stdoutReader and stderrReader:
      stdinWriter.close()
      // Delayed destroy, in case process doesn't shut down after stdin is closed:
      scheduleProcessDestruction(process, daqcore.defaults.defaultTimeout.duration)(actorSystem)
    }

    val stdoutReader = InputStreamReader(process.getInputStream, "stdoutReader")

    val stderrReader = InputStreamReader(process.getErrorStream, "stderrReader")
    stderrReader.recv(selfRef, StringLineCodec().dec, true)

    override def sync() = stdinWriter.sync()
    override def getSync() = stdinWriter.getSync()

    def flush = {}

    def send(data: ByteString) = stdinWriter.send(data)

    def send[A](data: A, encoder: Encoder[A]) = stdinWriter.send(data,encoder)

    def recv(): Future[ByteString] = stdoutReader.recv()

    def recv(receiver: ActorRef, repeat: Boolean) = stdoutReader.recv(receiver, repeat)

    def recv[A](decoder: Decoder[A]): Future[A] = stdoutReader.recv(decoder)

    def recv(receiver: ActorRef, decoder: Decoder[_], repeat: Boolean) = stdoutReader.recv(receiver, decoder, repeat)

    override def close() = {
      process.destroy()
      super.close()
    }


    val logExpr = """!?([^:]*):\s*(.*)""".r

    val stdErrLogPrefix = s"${args.head}: "
    def processStdErrLine(line: String): Unit = {
      line match {
        case logExpr("TRACE", msg) => log.trace(stdErrLogPrefix + msg)
        case logExpr("DEBUG", msg) => log.debug(stdErrLogPrefix + msg)
        case logExpr("INFO", msg)  => log.info(stdErrLogPrefix + msg)
        case logExpr("WARN", msg)  => log.warn(stdErrLogPrefix + msg)
        case logExpr("ERROR", msg) => log.error(stdErrLogPrefix + msg)
        case msg => log.warn(stdErrLogPrefix + msg)
      }
    }


    val stderrReaderARef = actorRef(stderrReader)

    override def receive = extend(super.receive) {
      case line: String if (context.sender == stderrReaderARef) => processStdErrLine(line)
      case x => log.warn(s"PROCESS STDERR: $x")
    }
  }


  object ProcessIOStreamImpl {
    protected def scheduleProcessDestruction(process: Process, delay: FiniteDuration)(implicit asys: ActorSystem): Unit =
      scheduleOnce(delay){ process.destroy() }
  }
}
