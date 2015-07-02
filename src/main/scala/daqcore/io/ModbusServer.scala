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
import scala.concurrent.{Future, Promise}
import scala.async.Async.{async, await}

import daqcore.util._
import daqcore.io._
import daqcore.actors._, daqcore.actors.TypedActorTraits._

import daqcore.io.prot.modbus._
import daqcore.io.prot.modbus.ModbusMsg._

import collection.immutable.Queue


trait ModbusServer {
  def query(req: ModbusReq): Future[ModbusResp]

  def readRegisterInput(slave: Int, address: Int): Future[Short]
  def readRegisterInputs(slave: Int, address: Int, count: Int): Future[ArrayVec[Short]]

  def readRegister(slave: Int, address: Int): Future[Short]
  def readRegisters(slave: Int, address: Int, count: Int): Future[ArrayVec[Short]]
  def writeRegister(slave: Int, address: Int, value: Short): Future[Short]
  def writeRegisters(slave: Int, address: Int, values: ArrayVec[Short]): Future[Int]

  def readBitInput(slave: Int, address: Int): Future[Boolean]
  def readBitInputs(slave: Int, address: Int, count: Int): Future[ArrayVec[Boolean]]

  def readBit(slave: Int, address: Int): Future[Boolean]
  def readBits(slave: Int, address: Int, count: Int): Future[ArrayVec[Boolean]]
  def writeBit(slave: Int, address: Int, value: Boolean): Future[Boolean]
  def writeBits(slave: Int, address: Int, value: ArrayVec[Boolean]): Future[Int]
}


trait ModbusASCIIServer extends ModbusServer 


object ModbusServer {
  def apply(uri: URI, name: String)(implicit rf: ActorRefFactory): ModbusServer = uri match {
    case HostURL("modbus-ascii", host, port) => typedActorOf[ModbusASCIIServer](new ASCIIImpl(HostURL("tcp", host, port).toString), name)
    case HostURL("modbus-tcp", host, port) => typedActorOf[ModbusASCIIServer](new TCPImpl(HostURL("tcp", host, port).toString), name)
    case uri => throw new IllegalArgumentException("URI \"%s\" not supported".format(uri))
  }

  def apply(uri: String, name: String = "")(implicit rf: ActorRefFactory): ModbusServer =
    apply(URI(uri), name)


  abstract class DefaultImpl extends ModbusServer
    with CloseableTAImpl with SyncableImpl with LocalECTypedActorImpl
  {
    val io: ByteStreamIO
    val codec: ModbusMasterCodec

    protected def queryImpl(req: ModbusReq): Future[ModbusResp]

    def query(req: ModbusReq): Future[ModbusResp] = {
      import daqcore.defaults.defaultTimeout
      log.trace(s"Request: ${req}")
      val respFt = queryImpl(req)
      localExec( async {
        val resp = await(respFt)
        log.trace(s"Response: $resp")
      } (_) )
      respFt
    }

    def checkedQuery[R](req: ModbusReq)(f: PartialFunction[ModbusResp, R]): Future[R] = {
      val mapFunc: Function[ModbusResp, R] = f orElse {
        case resp: ExceptionResp => throw new RuntimeException("Received modbus exception response: " + resp)
        case resp => throw new RuntimeException("Unexpected modbus response: " + resp)
      }

      localExec( async {
        mapFunc(await(query(req)))
      } (_) )
    }


    def readRegisterInput(slave: Int, address: Int): Future[Short] =
      readRegisterInputs(slave, address, 1).map{ _.head }(defaultExecContext)
    
    def readRegisterInputs(slave: Int, address: Int, count: Int): Future[ArrayVec[Short]] = {
      checkedQuery(ReadRegisterInputsReq(slave, address, count)) { _ match {
        case ReadRegisterInputsResp(`slave`, values) => values
      } }
    }


    def readRegister(slave: Int, address: Int): Future[Short] =
      readRegisters(slave, address, 1).map{ _.head }(defaultExecContext)

    def readRegisters(slave: Int, address: Int, count: Int): Future[ArrayVec[Short]] = {
      checkedQuery(ReadRegistersReq(slave, address, count)) { _ match {
        case ReadRegistersResp(`slave`, values) => values
      } }
    }

    def writeRegister(slave: Int, address: Int, value: Short): Future[Short] = {
      checkedQuery(WriteRegisterReq(slave, address, value)) { _ match {
        case WriteRegisterResp(`slave`, `address`, value) => value
      } }
    }

    def writeRegisters(slave: Int, address: Int, values: ArrayVec[Short]): Future[Int] = {
      checkedQuery(WriteRegistersReq(slave, address, values)) { _ match {
        case WriteRegistersResp(`slave`, `address`, count) => count
      } }
    }


    def readBitInput(slave: Int, address: Int): Future[Boolean] =
      readBitInputs(slave, address, 1).map{ _.head }(defaultExecContext)

    def readBitInputs(slave: Int, address: Int, count: Int): Future[ArrayVec[Boolean]] = {
      checkedQuery(ReadDiscreteInputsReq(slave, address, count)) { _ match {
        case ReadDiscreteInputsResp(`slave`, values) => values
      } }
    }


    def readBit(slave: Int, address: Int): Future[Boolean] =
      readBits(slave, address, 1).map{ _.head }(defaultExecContext)
    
    def readBits(slave: Int, address: Int, count: Int): Future[ArrayVec[Boolean]] = {
      checkedQuery(ReadCoilsReq(slave, address, count)) { _ match {
        case ReadCoilsResp(`slave`, values) => values
      } }
    }

    def writeBit(slave: Int, address: Int, value: Boolean): Future[Boolean] = {
      checkedQuery(WriteCoilReq(slave, address, value)) { _ match {
        case WriteCoilResp(`slave`, `address`, value) => value
      } }
    }

    def writeBits(slave: Int, address: Int, values: ArrayVec[Boolean]): Future[Int] = {
      checkedQuery(WriteCoilsReq(slave, address, values)) { _ match {
        case WriteCoilsResp(`slave`, `address`, count) => count
      } }
    }
  }

  class ASCIIImpl(val ioURI: String) extends ModbusServer.DefaultImpl with ModbusASCIIServer {
    import daqcore.defaults.defaultTimeout //!! get actor default timeout somehow?

    val io = ByteStreamIO(ioURI, "io-connection")
    val transport = ModbusASCIICodec
    val codec = ModbusMasterCodec(transport)

    def queryImpl(req: ModbusReq): Future[ModbusResp] = {
      io.send(req, codec.enc)
      val result = io.recv(codec.dec)
      // RS485 bus (or similar), have to wait for response
      result.get
      result
    }
  }

  class TCPImpl(val ioURI: String) extends ModbusServer.DefaultImpl with ModbusASCIIServer {
    val io = ByteStreamIO(ioURI, "io-connection")
    val transport = ModbusTCPCodec
    val codec = ModbusMasterCodec(transport)

    def queryImpl(req: ModbusReq): Future[ModbusResp] = {
      io.send(req, codec.enc)
      io.recv(codec.dec)
    }
  }
}
