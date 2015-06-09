// Copyright (C) 2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.devices

import scala.concurrent.{Future, Promise, ExecutionContext}

import daqcore.util._
import daqcore.io._
import daqcore.io.memory._
import daqcore.actors._


trait RegisterMemoryDevice[@specialized(Byte, Short, Int, Long) Value] extends Device with Syncable {
  import RegisterMemoryDevice.Address

  def read(address: Address): Future[Value]

  def write(address: Address, value: Value): Future[Unit]

  def partialRWWrite(address: Address, value: Value, bitMask: Value): Future[Unit]

  def partialJKWrite(address: Address, value: Value, bitMask: Value): Future[Unit]
}


object RegisterMemoryDevice {
  type Address = MemRegion.MemAddress

  class RegisterAccess[@specialized(Byte, Short, Int, Long) Value](protected val memDevice: RegisterMemoryDevice[Value]) {
    implicit class MemRegionSyncAccess(memRegion: MemRegion) {
      def sync()(implicit ctx: ExecutionContext): Future[Unit] = memDevice.getSync()
    }


    implicit class RegisterReadAccess(register: MemRegion#ReadableRegister[Value]) {
      def read()(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Value] =
        memDevice.read(register.absoluteAddress)
    }

    implicit class RegisterWriteAccess(register: MemRegion#ReadableRegister[Value]) {
      def write(value: Value)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
        memDevice.write(register.absoluteAddress, value)

      def write(value: register.FullValue)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
        write(value.value)
    }

    implicit class RWRegisterWriteAccess(register: MemRegion#RWRegister[Value]) {
      def partialWrite(value: Value, bitMask: Value)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
        memDevice.partialRWWrite(register.absoluteAddress, value, bitMask)
    }

    implicit class JKRegisterWriteAccess(register: MemRegion#JKRegister[Value]) {
      def partialWrite(value: Value, bitMask: Value)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] = {
        require(MemRegion.isValidJKValue(value), "Only lower half of bits is usable in a J/K register")
        memDevice.partialJKWrite(register.absoluteAddress, value, bitMask)
      }
    }

    implicit class RegisterSyncAccess(register: MemRegion#MemRegister[Value]) {
      def sync()(implicit ctx: ExecutionContext): Future[Unit] = memDevice.getSync()
    }


    implicit class BitSelectionReadAccess(bits: MemRegion#ReadableRegister[Value]#ReadableBitSelection[_]) {
      def read()(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Value] =
        memDevice.read(bits.register.absoluteAddress) map bits.getBits
    }

    implicit class RWBitSelectionWriteAccess(bits: MemRegion#RWRegister[Value]#DirectRWBitSelection[_]) {
      def write(value: Value)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
        memDevice.partialRWWrite(bits.register.absoluteAddress, bits.setBits(numType.zero, value), bits.bitMask)
    }

    implicit class JKBitSelectionWriteAccess(bits: MemRegion#JKRegister[Value]#JKBitSelection[_]) {
      def write(value: Value)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
        memDevice.partialJKWrite(bits.register.absoluteAddress, bits.setBits(numType.zero, value), bits.bitMask)
    }

    implicit class BitSelectionSyncAccess(bits: MemRegion#MemRegister[Value]#MemBitSelection[_]) {
      def sync()(implicit ctx: ExecutionContext): Future[Unit] = memDevice.getSync()
    }


    implicit class BitReadAccess(bit: MemRegion#ReadableRegister[Value]#ReadableBit[_]) {
      def read()(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Boolean] =
        memDevice.read(bit.register.absoluteAddress) map bit.getBit
    }

    implicit class RWBitWriteAccess(bit: MemRegion#RWRegister[Value]#DirectRWSingleBit[_]) {
      def set()(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
        memDevice.partialRWWrite(bit.register.absoluteAddress, bit.setBit(numType.zero), bit.bitMask)

      def clear()(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
        memDevice.partialRWWrite(bit.register.absoluteAddress, numType.zero, bit.bitMask)

      def write(value: Boolean)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
        if (value) set() else clear()
    }

    implicit class JKBitWriteAccess(bit: MemRegion#JKRegister[Value]#JKSingleBit[_]) {
      def set()(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
        memDevice.partialJKWrite(bit.register.absoluteAddress, bit.setBit(numType.zero), bit.bitMask)

      def clear()(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
        memDevice.partialJKWrite(bit.register.absoluteAddress, numType.zero, bit.bitMask)

      def write(value: Boolean)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
        if (value) set() else clear()
    }

    implicit class BitSyncAccess(bit: MemRegion#MemRegister[Value]#MemSingleBit[_]) {
      def sync()(implicit ctx: ExecutionContext): Future[Unit] = memDevice.getSync()
    }
  }


  implicit class ImplicitFunctions[@specialized(Byte, Short, Int, Long) Value](val memDevice: RegisterMemoryDevice[Value]) {
    def registerAccess = new RegisterAccess(memDevice)


    def read(register: MemRegion#ReadableRegister[Value]): Future[Value] = memDevice.read(register.absoluteAddress)

    def write(register: MemRegion#WriteableRegister[Value], value: Value)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
      memDevice.write(register.absoluteAddress, value)

    def write(value: MemRegion#MemRegister[Value]#FullValue)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
      memDevice.write(value.register.absoluteAddress, value.value)

    def write(register: MemRegion#KeyRegister[Value])(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
      memDevice.write(register.absoluteAddress, register.writeValue)

    def partialWrite(value: MemRegion#PartiallyWriteableRegister[Value]#PartialValue)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] = {
      value.register match {
        case register: MemRegion#RWRegister[Value] => memDevice.partialRWWrite(register.absoluteAddress, value.value, value.bitMask)
        case register: MemRegion#JKRegister[Value] => memDevice.partialJKWrite(register.absoluteAddress, value.value, value.bitMask)
      }
    }


    def readRaw(bits: MemRegion#ReadableRegister[Value]#ReadableBitSelection[_])(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Value] =
      memDevice.read(bits.register.absoluteAddress) map bits.getBits

    def readRaw(bit: MemRegion#ReadableRegister[Value]#ReadableBit[_])(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Boolean] =
      memDevice.read(bit.register.absoluteAddress) map bit.getBit

    def readConv[@specialized(Int, Long, Float, Double)  U](bits: MemRegion#ReadableRegister[Value]#ReadableBitSelection[U])(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[U] =
      memDevice.read(bits.register.absoluteAddress) map bits.getConvBits

    def writeRaw(bits: MemRegion#PartiallyWriteableRegister[Value]#WriteableBitSelection[_], value: Value)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
      partialWrite(bits ~/> value)

    def writeConv[@specialized(Int, Long, Float, Double)  U](bits: MemRegion#PartiallyWriteableRegister[Value]#WriteableBitSelection[U], value: U)(implicit ctx: ExecutionContext, numType: IntegerNumType[Value]): Future[Unit] =
      partialWrite(bits ~> value)
  }
}
