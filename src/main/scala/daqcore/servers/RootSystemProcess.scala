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

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._
import daqcore.util.fileops._
import daqcore.system._


class RootSystemProcess extends CloseableServer with RawMsgIO {
  rsp =>

  import RootSystemProcess.msgHeader

  class StdinWriter(output: OutputStream) extends CloseableServer with RawMsgOutput {
    override def init() = {
      super.init()
      addResource(output)
    }
    
    protected def srvSend(data: Seq[Byte]): Unit = {
      rsp.trace("srvSend(%s)".format(loggable(data map hex)))
      val msgData = data.toArray
      val msgLen = BigEndian.toBytes(Seq(msgData.length)).toArray
      output.write(msgHeader.toArray)
      output.write(msgLen)
      output.write(msgData)
    }

    protected def srvFlush(): Unit = {
      rsp.trace("srvFlush()")
      output.flush()
    }

    override protected def srvClose() = {
      srvFlush()
      super.srvClose()
    }
    
    override def serve = super.serve orElse {
      case RawMsgOutput.Send(data) => srvSend(data)
      case RawMsgOutput.Flush() => srvFlush()
    }
  }


  class StdoutReader(input: InputStream) extends CloseableServer with RawMsgInput {
    override def init() = {
      super.init()
      addResource(input)
    }

    protected def srvRecv(): Unit = {
      import scala.collection.immutable.Queue
      
      def read(trg: Array[Byte]): Unit = {
        rsp.trace("srvRecv(): reading %s bytes.".format(trg.length))
        val count = input.read(trg)
        if (count == -1) { rsp.close() }
        else assert { count == trg.length }
      }

      def readByte = { val a = Array.ofDim[Byte](1); read(a); a(0) }
      
      rsp.trace("srvRecv()")
      try {
        val lenData = Array.ofDim[Byte](4)
        var header = Queue(readByte, readByte)
        rsp.trace("srvRecv(): Waiting for message header")
        while (header.toList != msgHeader) header = { header.enqueue(readByte).drop(1) }
        rsp.trace("srvRecv(): Received valid message header")
        read(lenData)
        val len = BigEndian.fromBytes[Int](lenData).head
        val msgData = Array.ofDim[Byte](len)
        read(msgData)
        rsp.trace("received(%s byte(s): %s)".format(msgData.length, loggable(msgData.toSeq map hex)))
        reply(ByteStreamInput.Received(msgData))
      } catch {
        case e: IOException => srvClose()
      }
    }
    
    override def serve = super.serve orElse {
      case RawMsgInput.Recv() => srvRecv()
    }
  }

  var process: Process = null
  var stdin: OutputStream = null
  var stdout: InputStream = null
  var stderr: InputStream = null
  
  var stdinWriter: StdinWriter = null
  var stdoutReader: StdoutReader = null
  
  var tmpRootIOSrc: File = null

  override def init() = {
    super.init()

    val dbgConfig = Seq(
      isTraceEnabled -> "TRACE",
      isDebugEnabled -> "DEBUG",
      isInfoEnabled -> "INFO",
      isWarnEnabled -> "WARN",
      isErrorEnabled -> "ERROR"
    )
    val dbgDefs = (for { (on, name) <- dbgConfig if (on) } yield {"#define %s\n".format(name)}).mkString.getBytes("ASCII")
    tmpRootIOSrc = RootSystemProcess.tmpResourceCopy("/cxx/root-system/rootSysServer.cxx", before = dbgDefs)
    
    process = new ProcessBuilder("root", "-l", "-b", "-q", tmpRootIOSrc.getPath+"+").start
    stdin = process.getOutputStream
    stdout = process.getInputStream
    stderr = process.getErrorStream
    Seq(stdin, stdout, stderr) foreach addResource
    
    stdinWriter = new StdinWriter(stdin)
    link(stdinWriter)
    stdinWriter.start
    stdoutReader = new StdoutReader(stdout)
    link(stdoutReader)
    stdoutReader.start

    spawn {
      import java.io.{InputStreamReader, BufferedReader}

      val logExpr = """([^:]*):\s*(.*)""".r
      val in = new BufferedReader(new InputStreamReader(stderr))
      try {
        while (true) in.readLine match {
          case null => {
            rsp.close()
            throw new IOException("EOF")
          }
          case logExpr("TRACE", msg) => trace("ROOT: " + msg)
          case logExpr("DEBUG", msg) => debug("ROOT: " + msg)
          case logExpr("INFO", msg)  => info("ROOT: " + msg)
          case logExpr("WARN", msg)  => warn("ROOT: " + msg)
          case logExpr("ERROR", msg) => error("ROOT: " + msg)
          case msg => info("ROOT: " + msg)
        }
      }
      catch { case e: IOException => }
    }

    Seq(stdinWriter, stdoutReader) foreach addResource
  }

  override def serve = super.serve orElse {
    case op: RawMsgOutput.Send => stdinWriter.forward(op)
    case op: RawMsgOutput.Flush => stdinWriter.forward(op)
    case op: RawMsgInput.Recv => stdoutReader.forward(op)
  }
}


object RootSystemProcess extends Logging {
  val msgHeader = List(0x10.toByte, 0x9B.toByte) // DLE CSI
  
  val rootExe = new java.io.File((Nil | "root-config --bindir").head + "/root.exe")

  assert(rootExe.exists)
  
  val rootBuildID = {
    val rootBuildSpec = Seq(rootExe.getAbsolutePath, rootExe.length, rootExe.lastModified).mkString(",")
    val rootBuildNs = java.util.UUID.fromString("b056326c-fd18-4335-bdaf-5c32b05c63b9")
    Version5UUID(rootBuildNs, rootBuildSpec)
  }
  
  def tmpResourceCopy(resourceName: String, before: Seq[Byte] = Nil, after: Seq[Byte] = Nil): File = {
    val name = new File(resourceName) getName
    val srcBytes = (before ++ this.getClass.getResource(resourceName).getBytes.toSeq ++ after).toArray
    val userName = java.lang.System.getProperty("user.name")
    val uuid = Version5UUID(rootBuildID, srcBytes)
    val trgDir = daqcoreTmpDir / uuid.toString
    val tmpCopy = trgDir / name
    if (! tmpCopy.exists) {
      debug("Creating temporary resource \"%s\"".format(tmpCopy))
      trgDir.mkdirs()
      tmpCopy writeBytes srcBytes
    }
    tmpCopy
  }
  
  def apply(): RawMsgIO = (new RootSystemProcess() start).asInstanceOf[RawMsgIO]
}
