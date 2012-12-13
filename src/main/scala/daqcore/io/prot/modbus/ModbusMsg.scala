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


package daqcore.io.prot.modbus

import java.io.{DataInputStream, DataOutputStream}

import net.wimpi.modbus.{msg => jamsg}
import net.wimpi.modbus.{procimg => jaimg}

import daqcore.io._
import daqcore.util._


trait ModbusMsg extends Product with HasByteRep {
  def slave: Int
}


trait ModbusReq extends ModbusMsg

object ModbusReq {
  def apply(bytes: ByteString): ModbusReq = apply(bytes.iterator)

  def apply(iterator: ByteIterator): ModbusReq = {
    import ModbusMsg._

    val iterator2 = iterator.clone
    val slave = iterator2.getByte.toInt & 0xff
    val function = iterator2.getByte.toInt & 0xff
    val data = iterator2.toByteString

    val request = jamsg.ModbusRequest.createModbusRequest(function)
    request.setHeadless()
    request.readFrom(new DataInputStream(iterator.asInputStream))
    
    request match {
      case msg: jamsg.ReadInputDiscretesRequest =>
        ReadDiscreteInputsReq(msg.getUnitID, msg.getReference, msg.getBitCount)
      case msg: jamsg.ReadCoilsRequest =>
        ReadCoilsReq(msg.getUnitID, msg.getReference, msg.getBitCount)
      case msg: jamsg.WriteCoilRequest =>
        WriteCoilReq(msg.getUnitID, msg.getReference, msg.getCoil)
      case msg: jamsg.ReadInputRegistersRequest =>
        ReadRegisterInputsReq(msg.getUnitID, msg.getReference, msg.getWordCount)
      case msg: jamsg.ReadMultipleRegistersRequest =>
        ReadRegistersReq(msg.getUnitID, msg.getReference, msg.getWordCount)
      case msg: jamsg.WriteSingleRegisterRequest =>
        WriteRegisterReq(msg.getUnitID, msg.getReference, msg.getRegister.getValue.toShort)
      case msg: jamsg.WriteMultipleCoilsRequest =>
        WriteCoilsReq(msg.getUnitID, msg.getReference, ArrayVec( ((0 to msg.getCoils.size-1) map msg.getCoils.getBit) : _*) )
      case msg: jamsg.WriteMultipleRegistersRequest =>
        WriteRegistersReq(msg.getUnitID, msg.getReference, ArrayVec( ((0 to msg.getWordCount-1) map {i => msg.getRegister(i).getValue.toShort}) : _*) )
      case msg: jamsg.IllegalFunctionRequest =>
        RawReq(slave, function, data)
      case msg => throw new IllegalArgumentException("Modbus message of type %s can't be interpreted as a request".format(msg.getClass.getName))
    }
  }
}


trait ModbusResp extends ModbusMsg

object ModbusResp {
  def apply(bytes: ByteString): ModbusResp = apply(bytes.iterator)

  def apply(iterator: ByteIterator): ModbusResp = {
    import ModbusMsg._

    val iterator2 = iterator.clone
    val slave = iterator2.getByte.toInt & 0xff
    val function = iterator2.getByte.toInt & 0xff
    val data = iterator2.toByteString
    
    val response = jamsg.ModbusResponse.createModbusResponse(function)
    response.setHeadless()
    response.readFrom(new DataInputStream(iterator.asInputStream))
    
    response match {
      case msg: jamsg.ReadInputDiscretesResponse =>
        ReadDiscreteInputsResp(msg.getUnitID, ArrayVec( ((0 to msg.getDiscretes.size-1) map msg.getDiscretes.getBit) : _*))
      case msg: jamsg.ReadCoilsResponse =>
        ReadCoilsResp(msg.getUnitID, ArrayVec( ((0 to msg.getCoils.size-1) map msg.getCoils.getBit) : _*))
      case msg: jamsg.WriteCoilResponse =>
        WriteCoilResp(msg.getUnitID, msg.getReference, msg.getCoil)
      case msg: jamsg.ReadInputRegistersResponse =>
        ReadRegisterInputsResp(msg.getUnitID, ArrayVec( ((0 to msg.getWordCount-1) map {i => msg.getRegisterValue(i).toShort}) : _*) )
      case msg: jamsg.ReadMultipleRegistersResponse =>
        ReadRegistersResp(msg.getUnitID, ArrayVec( ((0 to msg.getWordCount-1) map {i => msg.getRegisterValue(i).toShort}) : _*) )
      case msg: jamsg.WriteSingleRegisterResponse =>
        WriteRegisterResp(msg.getUnitID, msg.getReference, msg.getRegisterValue.toShort)
      case msg: jamsg.WriteMultipleCoilsResponse =>
        WriteCoilsResp(msg.getUnitID, msg.getReference, msg.getBitCount)
      case msg: jamsg.WriteMultipleRegistersResponse =>
        WriteRegistersResp(msg.getUnitID, msg.getReference, msg.getWordCount)
      case msg: jamsg.ExceptionResponse => {
        if ((function & 0x80) != 0) ExceptionResp(msg.getUnitID, msg.getFunctionCode & 0x7f, msg.getExceptionCode)
        else RawResp(slave, function, data)
      }
      case msg => throw new IllegalArgumentException("Modbus message of type %s can't be interpreted as a response".format(msg.getClass.getName))
    }
  }
}

object ModbusMsg {
  private def putMsgBytes(builder: ByteStringBuilder, slave: Int, msg: jamsg.ModbusMessageImpl): Unit = {
    msg.setUnitID(slave)
    msg.setHeadless()
    msg.writeTo(new DataOutputStream(builder.asOutputStream))
  }
  
  trait NormalReq extends ModbusReq { val address: Int }
  trait NormalResp extends ModbusResp
 
  trait ModbusWrite extends NormalReq
  trait ModbusRead extends NormalReq

  trait ModbusWriteResp extends NormalResp
  trait ModbusReadResp extends NormalResp
  
  
  trait MobusSingleRegMsg {
    val value: Short
  }

  trait MobusMultRegMsg {
    val values: ArrayVec[Short]
  }

  // Modbus function code 2
  case class ReadDiscreteInputsReq(slave: Int, address: Int, count: Int) extends ModbusRead
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.ReadInputDiscretesRequest(address, count)) }
  case class ReadDiscreteInputsResp(slave: Int, values: ArrayVec[Boolean]) extends ModbusReadResp {
    def putBytes(builder: ByteStringBuilder) = {
      val msg = new jamsg.ReadInputDiscretesResponse(values.length)
      for (i <- 0 to values.length-1) msg.setDiscreteStatus(i, values(i))
      putMsgBytes(builder, slave, msg)
    }
  }

  // Modbus function code 1
  case class ReadCoilsReq(slave: Int, address: Int, count: Int) extends ModbusRead
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.ReadCoilsRequest(address, count)) }
  case class ReadCoilsResp(slave: Int, values: ArrayVec[Boolean]) extends ModbusReadResp {
    def putBytes(builder: ByteStringBuilder) = {
      val msg = new jamsg.ReadCoilsResponse(values.length)
      for (i <- 0 to values.length-1) msg.setCoilStatus(i, values(i))
      putMsgBytes(builder, slave, msg)
    }
  }

  // Modbus function code 5
  case class WriteCoilReq(slave: Int, address: Int, value: Boolean) extends ModbusWrite
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.WriteCoilRequest(address, value)) }
  case class WriteCoilResp(slave: Int, address: Int, value: Boolean) extends ModbusWriteResp
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.WriteCoilResponse(address, value)) }

  // Modbus function code 4
  case class ReadRegisterInputsReq(slave: Int, address: Int, count: Int) extends ModbusRead
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.ReadInputRegistersRequest(address, count)) }
  case class ReadRegisterInputsResp(slave: Int, values: ArrayVec[Short]) extends ModbusReadResp with MobusMultRegMsg
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.ReadInputRegistersResponse((values map {v => new jaimg.SimpleRegister(v.toInt)}).toArray)) }

  // Modbus function code 3
  case class ReadRegistersReq(slave: Int, address: Int, count: Int) extends ModbusRead
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.ReadMultipleRegistersRequest(address, count)) }
  case class ReadRegistersResp(slave: Int, values: ArrayVec[Short]) extends ModbusReadResp with MobusMultRegMsg
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.ReadMultipleRegistersResponse((values map {v => new jaimg.SimpleRegister(v.toInt)}).toArray)) }

  // Modbus function code 6
  case class WriteRegisterReq(slave: Int, address: Int, value: Short) extends ModbusWrite with MobusSingleRegMsg
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.WriteSingleRegisterRequest(address, new jaimg.SimpleRegister(value.toInt))) }
  case class WriteRegisterResp(slave: Int, address: Int, value: Short) extends ModbusWriteResp with MobusSingleRegMsg
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.WriteSingleRegisterRequest(address, new jaimg.SimpleRegister(value.toInt))) }

  // Modbus function code 15
  case class WriteCoilsReq(slave: Int, address: Int, values: ArrayVec[Boolean]) extends ModbusWrite {
    def putBytes(builder: ByteStringBuilder) = {
      val msg = new jamsg.WriteMultipleCoilsRequest(address, values.length)
      for (i <- 0 to values.length-1) msg.setCoilStatus(i, values(i))
      putMsgBytes(builder, slave, msg)
    }
  }
  case class WriteCoilsResp(slave: Int, address: Int, count: Int) extends ModbusWriteResp
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.WriteMultipleCoilsResponse(address, count)) }

  // Modbus function code 16
  case class WriteRegistersReq(slave: Int, address: Int, values: ArrayVec[Short]) extends ModbusWrite with MobusMultRegMsg
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.WriteMultipleRegistersRequest(address, (values map {v => new jaimg.SimpleRegister(v.toInt)}).toArray)) }
  case class WriteRegistersResp(slave: Int, address: Int, count: Int) extends ModbusWriteResp
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.WriteMultipleRegistersResponse(address, count)) }
  
  case class ExceptionResp(slave: Int, function: Int, code: Int) extends ModbusResp
    { def putBytes(builder: ByteStringBuilder) = putMsgBytes(builder, slave, new jamsg.ExceptionResponse(function, code)) }
  
  case class RawReq(slave: Int, function: Int, data: ByteString) extends ModbusReq
    { def putBytes(builder: ByteStringBuilder) = builder.putByte(slave.toByte).putByte(function.toByte).++=(data) }
  case class RawResp(slave: Int, function: Int, data: ByteString) extends ModbusResp
    { def putBytes(builder: ByteStringBuilder) = builder.putByte(slave.toByte).putByte(function.toByte).++=(data) }
}
