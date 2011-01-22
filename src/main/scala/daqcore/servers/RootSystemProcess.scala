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


package daqcore.servers

import scala.tools.nsc.io.Process.Pipe._

import java.io.{File, InputStream, OutputStream, IOException}
import java.lang.{Process}

import akka.actor._, akka.actor.Actor._, akka.dispatch.Future
import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle, Temporary, OneForOneStrategy, AllForOneStrategy}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._
import daqcore.util.fileops._
import daqcore.system._


class RootSystemProcess extends Server with KeepAlive with PostInit with CloseableServer {
  import RootSystemProcess.{msgHeader}
  implicit val encoding = BigEndian

  val rsp: ActorRef = self
  val rspLog = log

  override def profiles = super.profiles.+[RawMsgIO]

  class EOIException extends Throwable
  case class EndOfInput()


  class StdinWriter(output: OutputStream) extends CascadableServer {
    override def profiles = super.profiles.+[RawMsgOutput]

    override def init() = {
      super.init()
      self.lifeCycle = Temporary
      atCleanup { try { srvFlush() } finally { output.close } }
    }
    
    protected def srvSend(data: ByteSeq): Unit = {
      log.trace("srvSend(%s)".format(loggable(data map hex)))
      val bld = ByteSeqBuilder()

      bld ++= msgHeader
      bld.putInt(data.length)
      bld ++= data

      output.write(bld.result().toArray)
    }

    protected def srvFlush(): Unit = {
      log.trace("srvFlush()")
      output.flush()
    }
    
    override def serve = super.serve orElse {
      case RawMsgOutput.Send(data) => srvSend(data)
      case RawMsgOutput.Flush() => srvFlush()
    }
  }


  class StdoutReader(input: InputStream) extends CascadableServer {
    override def profiles = super.profiles.+[RawMsgInput]

    override def init() = {
      super.init()
      self.lifeCycle = Temporary
      atCleanup { input.close() }
    }
    
    protected def srvRecv(): Unit = {
      import scala.collection.immutable.Queue
      
      def read(trg: Array[Byte]): Unit = {
        // log.trace("srvRecv(): reading %s bytes.".format(trg.length))
        var pos = 0;
        while (pos < trg.length) {
          val count = input.read(trg, pos, trg.length - pos)
          if (count == -1) throw new EOIException
          else {
            assert(count > 0)
            pos += count
          }
        }
      }

      def readByte = { val a = Array.ofDim[Byte](1); read(a); a(0) }
      
      log.trace("srvRecv()")
      try {
        val lenData = Array.ofDim[Byte](4)
        var header = Queue(readByte, readByte)
        log.trace("srvRecv(): Waiting for message header")
        while (header.toList != msgHeader) header = { header.enqueue(readByte).drop(1) }
        log.trace("srvRecv(): Received valid message header")
        read(lenData)
        val len = BigEndian.fromBytes[Int](lenData.toArrayVec).head
        val msgData = Array.ofDim[Byte](len)
        read(msgData)
        log.trace("received(%s byte(s): %s)".format(msgData.length, loggable(msgData.toSeq map hex)))
        reply(ByteStreamInput.Received(ByteSeq.wrap(msgData)))
      } catch {
        case e: EOIException => {
            log.debug("Reached end of STDIN")
            // StderrReader will send EndOfInput to RootSystemProcess, so do nothing here
            self.stop()
        }
      }
    }

    override def serve = super.serve orElse {
      case RawMsgInput.Recv() => srvRecv()
    }
  }
  

  class StderrReader(input: InputStream) extends CascadableServer with PostInit {
    import java.io.{InputStreamReader, BufferedReader}

    val logExpr = """([^:]*):\s*(.*)""".r
    val in = new BufferedReader(new InputStreamReader(input))

    override def init() = {
      super.init()
      self.lifeCycle = Temporary
      atCleanup { input.close() }
    }
    
    override def postInit() = {
      super.postInit()

      try {
        while (true) in.readLine match {
          case null => throw new EOIException
          case logExpr("TRACE", msg) => rspLog.trace("ROOT: " + msg)
          case logExpr("DEBUG", msg) => rspLog.debug("ROOT: " + msg)
          case logExpr("INFO", msg)  => rspLog.info("ROOT: " + msg)
          case logExpr("WARN", msg)  => rspLog.warn("ROOT: " + msg)
          case logExpr("ERROR", msg) => rspLog.error("ROOT: " + msg)
          case msg => rspLog.info("ROOT: " + msg)
        }
      }
      catch {
        case e: EOIException => {
            log.debug("Reached end of STDERR")
            try { rsp ! EndOfInput() } catch { case e: ActorInitializationException => }
            self.stop()
        }
        case e => {
          log.error(e.toString)
          throw e
        }
      }
    }
  }


  var process: Process = null
  
  var stdinWriter: ActorRef = null
  var stdoutReader: ActorRef = null
  var stderrReader: ActorRef = null
  
  var tmpRootIOSrc: File = null


  override def init() = {
    super.init()

    self.faultHandler = AllForOneStrategy(List(classOf[Throwable]), 3, 1000)

    tmpRootIOSrc = RootSystemProcess.tmpResourceCopy("/cxx/root-system/rootSysServer.cxx")
  }

  override def postInit() = {
    import scala.collection.JavaConversions._

    super.postInit()
    withCleanup {
      val logLevel: Int = (
        if (rspLog.trace_?) 1000
        else if (rspLog.debug_?) 2000
        else if (rspLog.info_?) 3000
        else if (rspLog.warning_?) 4000
        else if (rspLog.error_?) 5000
        else 0x7fffffff
      )

      val pb = new ProcessBuilder("root", "-l", "-b", "-q", "%s+(%s)".format(tmpRootIOSrc.getPath, logLevel))
      log.debug("Starting new ROOT-System process: " + pb.command.mkString(" "))
      process = pb.start
      
      // Since these actors are temporary, they have to be started in postInit to prevent them
      // from being shut down again instantly on a restart
      stdinWriter = rsp.linkStart(actorOf(new StdinWriter(process.getOutputStream)))
      stdoutReader = rsp.linkStart(actorOf(new StdoutReader(process.getInputStream)))
      stderrReader = rsp.linkStart(actorOf(new StderrReader(process.getErrorStream)))
    } {
      // Explicit shutdown of stdin/-out/-err handlers, since shutdown order
      // is important:
      // stdinWriter has to be stopped first, triggering a shutdown of the
      // ROOT process. Shutdown of stdoutReader/stderrReader may/will
      // block until ROOT process has exited, closing it's stdout and stderr.
      stdinWriter.stop(); stdinWriter = null
      stdoutReader.stop(); stdoutReader = null
      stderrReader.stop(); stderrReader = null
    }
  }

  override def serve = super.serve orElse {
    case op: RawMsgOutput.Send => stdinWriter ! op
    case op: RawMsgOutput.Flush => stdinWriter ! op
    case op: RawMsgInput.Recv => stdoutReader forward op
    case op: EndOfInput => {
      val s = self.getSender.get
      if (stderrReader == null) log.error("stderrReader == null")
      else if (s == stderrReader) // Discard notifications from old instances
        throw new java.io.IOException("ROOT-System process closed unexpectedly")
    }
  }
}


object RootSystemProcess extends Logging {
  val msgHeader = ByteSeq(0x10.toByte, 0x9B.toByte) // DLE CSI
  
  val rootExe = new java.io.File((Nil | "root-config --bindir").head + "/root.exe")

  assert(rootExe.exists)
  
  val rootBuildID = {
    val rootBuildSpec = Seq(rootExe.getAbsolutePath, rootExe.length, rootExe.lastModified).mkString(",")
    val rootBuildNs = java.util.UUID.fromString("b056326c-fd18-4335-bdaf-5c32b05c63b9")
    Version5UUID(rootBuildNs, rootBuildSpec)
  }
  
  def tmpResourceCopy(resourceName: String): File = {
    val name = new File(resourceName) getName
    val srcBytes = this.getClass.getResource(resourceName).getBytes
    val userName = java.lang.System.getProperty("user.name")
    val uuid = Version5UUID(rootBuildID, srcBytes)
    val trgDir = daqcoreTmpDir / uuid.toString
    val tmpCopy = trgDir / name
    if (! tmpCopy.exists) {
      log.debug("Creating temporary resource \"%s\"".format(tmpCopy))
      trgDir.mkdirs()
      tmpCopy writeBytes srcBytes
    }
    tmpCopy
  }
  
  def apply(sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): RawMsgIO =
    new ServerProxy(sv.linkStart(actorOf(new RootSystemProcess()), lc)) with RawMsgIO
}
