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

import scala.language.postfixOps

import akka.actor._
import scala.concurrent.{Future, Promise, ExecutionContext}

import java.nio.{ByteOrder => NIOByteOrder}

import daqcore.util._
import daqcore.io._
import daqcore.actors._, daqcore.actors.TypedActorTraits._


trait EPOSCANOpenGateway extends CANOpenServer {
  import EPOSCANOpenGateway.SerialMsg

  def command(msg: SerialMsg): Future[Unit]
  def query(msg: SerialMsg): Future[ByteString]
}



object EPOSCANOpenGateway {
  def apply(uri: URI, name: String)(implicit rf: ActorRefFactory): EPOSCANOpenGateway = uri match {
    case HostURL("epos-canopen", host, port) => typedActorOf[EPOSCANOpenGateway](new GatewayImpl(HostURL("tcp", host, port).toString), name)
    case uri => throw new IllegalArgumentException("URI \"%s\" not supported".format(uri))
  }

  def apply(uri: String, name: String = "")(implicit rf: ActorRefFactory): EPOSCANOpenGateway =
    apply(URI(uri), name)


  case class SerialMsg(opCode: Byte, data: ByteString) {
    import SerialMsg.crcGen

    require (data.length >=1)
    require (data.length % 2 == 0)

    def crc: Short = {
      var crc: Short = 0
      crc = crcGen.update(opCode, crc)
      crc = crcGen.update((data.length/2 - 1).toByte, crc)
      val dataBytes = data.iterator
      while (!dataBytes.isEmpty) {
        val (low, high) = (dataBytes.next(), dataBytes.next())
        // data is interpreted as 16-bit words, crc is computed high byte first
        crc = crcGen.update(high, crc)
        crc = crcGen.update(low, crc)
      }
      crc
    }
  }

  object SerialMsg {
    val crcGen = CRC_CCITT
  }


  object SerialCodec extends Codec[SerialMsg, SerialMsg] {
    def byteOrder = LittleEndian
    implicit def nioByteOrder = byteOrder.nioByteOrder

    private def encOpCodeFct(out: ByteStringBuilder, msg: SerialMsg): Unit = {
      assert(msg.data.length <= 0xff + 1)
      out.putByte(msg.opCode)
    }
    val encOpCode = encOpCodeFct(_, _)

    private def encDataAndCRCFct(out: ByteStringBuilder, msg: SerialMsg): Unit = {
      assert(msg.data.length <= 0xff + 1)

      out.putByte((msg.data.length/2 - 1).toByte)
      for {x <- msg.data} out.putByte(x)
      out.putShort(msg.crc)
    }
    val encDataAndCRC = encDataAndCRCFct(_, _)

    private def encFct(out: ByteStringBuilder, msg: SerialMsg): Unit = {
      encOpCode(out, msg)
      encDataAndCRCFct(out, msg)
    }
    val enc = encFct(_, _)


    val decOpCode = for {
      opCodeRaw <- Decoder take 1
      opCode = opCodeRaw.head
    } yield opCode

    val decDataAndCRC = for {
      lenMinus1Raw <- (Decoder take 1)
      dataLen = ((lenMinus1Raw.head.toInt) & 0xff) + 1
      data <- Decoder take dataLen * 2
      crcBytes <- Decoder take 2
      crc = byteOrder.getShort(crcBytes.iterator)
    } yield (data, crc)

    val dec = for {
      opCode <- decOpCode
      dataAndCRC <- decDataAndCRC
    } yield {
      val (data, crc) = dataAndCRC
      val msg = SerialMsg(opCode, data)
      require (msg.crc == crc)
      msg
    }
  }


  class GatewayImpl(val ioURI: String) extends EPOSCANOpenGateway with CloseableTAImpl with SyncableImpl {
    import GatewayImpl._
    import daqcore.defaults.defaultTimeout //!! get actor default timeout somehow?
    implicit def nioByteOrder = byteOrder.nioByteOrder

    val io = ByteStreamIO(ioURI, "io-connection")

    def command(msg: SerialMsg): Future[Unit] = {
      io.send(msg, SerialCodec.encOpCode)
      if (io.recv(SerialCodec.decOpCode).get != ack) throw new RuntimeException("Received NACK")
      io.send(msg, SerialCodec.encDataAndCRC)
      if (io.recv(SerialCodec.decOpCode).get != ack) throw new RuntimeException("Received NACK")
      Promise successful {} future
    }

    def query(msg: SerialMsg): Future[ByteString] = {
      command(msg).get

      val revcOpCode = io.recv(SerialCodec.decOpCode).get
      io.send(ByteString(ack))

      val (recvData, recvCRC) = io.recv(SerialCodec.decDataAndCRC).get
      val recvMsg = SerialMsg(revcOpCode, recvData)
      val crcOK = recvMsg.crc == recvCRC
      io.send(ByteString(if (crcOK) ack else nack))

      if (!crcOK) throw new RuntimeException("CRC error")
      else if (revcOpCode != opCodeResponse) throw new RuntimeException("Received unexpexted opcode 0x%02x in response".format(revcOpCode))
      else Promise successful recvMsg.data future
    }

    def readObject(node: Int, idx: Int, subIdx: Int): Future[ByteString] = {
      val cmd = SerialMsg(opCodeReadObject, encodeODIdx(ByteString.newBuilder, node, idx, subIdx).result)
      val resp = query(cmd).get
      val respData = resp.iterator
      val error = respData.getInt
      val value = respData.toByteString
      if (error != 0) Promise failed (new RuntimeException("EPOS error 0x%08x".format(error))) future
      else Promise successful value future
    }

    def readByteObject(node: Int, idx: Int, subIdx: Int): Future[Byte] =
        getByte(readObject(node, idx, subIdx))(defaultExecContext)

    def readShortObject(node: Int, idx: Int, subIdx: Int): Future[Short] =
        getShort(readObject(node, idx, subIdx))(defaultExecContext, nioByteOrder)

    def readIntObject(node: Int, idx: Int, subIdx: Int): Future[Int] =
        getInt(readObject(node, idx, subIdx))(defaultExecContext, nioByteOrder)

    def writeObject(node: Int, idx: Int, subIdx: Int, value: ByteString): Future[Unit] = {
      val v = if (value.length % 2 != 0) value ++ ByteString(0) else value
      val cmd = SerialMsg(opCodeWriteObject, encodeODIdx(ByteString.newBuilder, node, idx, subIdx).++=(v).result)
      val resp = query(cmd).get
      val error = resp.iterator.getInt
      if (error != 0) Promise failed (new RuntimeException("EPOS error 0x%08x".format(error))) future
      else Promise successful {} future
    }

    def writeByteObject(node: Int, idx: Int, subIdx: Int, value: Byte): Future[Unit] =
      writeObject(node, idx, subIdx, ByteString.newBuilder.putByte(value).result)

    def writeShortObject(node: Int, idx: Int, subIdx: Int, value: Short): Future[Unit] =
      writeObject(node, idx, subIdx, ByteString.newBuilder.putShort(value).result)

    def writeIntObject(node: Int, idx: Int, subIdx: Int, value: Int): Future[Unit] =
      writeObject(node, idx, subIdx, ByteString.newBuilder.putInt(value).result)
  }

  object GatewayImpl {
    def byteOrder = LittleEndian

    val ack: Byte = 0x4f
    val nack: Byte = 0x46

    val opCodeResponse: Byte = 0x00
    val opCodeReadObject: Byte = 0x10
    val opCodeWriteObject: Byte = 0x11

    def fitsInByte(x: Int) = ((x & 0xff) == x)

    def fitsInShort(x: Int) = ((x & 0xffff) == x)

    def encodeODIdx(builder: ByteStringBuilder, node: Int, idx: Int, subIdx: Int): ByteStringBuilder = {
      implicit def nioByteOrder = byteOrder.nioByteOrder

      require (fitsInByte(node))
      require (fitsInShort(idx))
      require (fitsInByte(subIdx))

      builder.putShort(idx.toShort)
      builder.putByte(subIdx.toByte)
      builder.putByte(node.toByte)
    }

    protected def getByte(ftBytes: Future[ByteString])(implicit executor: ExecutionContext) =
      ftBytes map { _.iterator.getByte }

    protected def getShort(ftBytes: Future[ByteString])(implicit executor: ExecutionContext, byteOrder: NIOByteOrder) =
      ftBytes map { _.iterator.getShort }

    protected def getInt(ftBytes: Future[ByteString])(implicit executor: ExecutionContext, byteOrder: NIOByteOrder) =
      ftBytes map { _.iterator.getInt }
  }
}
