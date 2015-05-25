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
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import scala.collection.immutable.Queue

import daqcore.actors._
import daqcore.util._
import daqcore.io._
import daqcore.io.memory._


trait SIS3316Memory extends RegisterMemoryDevice[Int] {
  import RegisterMemoryDevice.Address

  def vme: Future[VMEBus]

  def bulkReadEncoding: Future[ValEncoding]
  def bulkWriteEncoding: Future[ValEncoding]

  def readBulk(address: Address, nBytes: Int): Future[ByteString]

  def writeBulk(address: Address, data: ByteString): Future[Unit]
}


object SIS3316Memory extends DeviceCompanion[SIS3316Memory] {
  def impl = { case uri => new SIS3316MemoryImpl(uri.toString) }

  class SIS3316MemoryImpl(vmeURI: String) extends SIS3316Memory
    with CloseableTAImpl with SyncableImpl
  {
    import RegisterMemoryDevice.Address
    import SIS3316MemoryImpl._

    val vmeBus = VMEBus(vmeURI, "vme")

    var registerCache = MemValues[Address, Int]()


    def identity = successful("SIS3316Memory")

    def vme = successful(vmeBus)

    def bulkReadEncoding = vmeBus.bulkEncoding(bulkReadMode, deviceByteOrder)
    def bulkWriteEncoding = vmeBus.bulkEncoding(bulkWriteMode, deviceByteOrder)


    override def sync() = {
      actionsQueue.add(SyncAction())
    }


    override def getSync() = {
      val resultPromise = Promise[Unit]()
      actionsQueue.add(SyncAction(List(resultPromise success _)))
      resultPromise.future
    }


    def read(address: Address) = {
      val resultPromise = Promise[Int]()
      actionsQueue.add(RegisterActions.read(address){resultPromise success _})
      resultPromise.future
    }


    def write(address: Address, value: Int) = {
      val resultPromise = Promise[Unit]()
      actionsQueue.add(RegisterActions.write(address, value){resultPromise success _})
      resultPromise.future
    }


    def partialRWWrite(address: Address, value: Int, bitMask: Int) = {
      val resultPromise = Promise[Unit]()
      actionsQueue.add(RegisterActions.partialRWWrite(address, value, bitMask){resultPromise success _})
      resultPromise.future
    }


    def partialJKWrite(address: Address, value: Int, bitMask: Int) = {
      val resultPromise = Promise[Unit]()
      actionsQueue.add(RegisterActions.partialJKWrite(address, value, bitMask){resultPromise success _})
      resultPromise.future
    }


    def readBulk(address: Address, nBytes: Int) = {
      val resultPromise = Promise[ByteString]()
      actionsQueue.add(BulkRead(address, nBytes){resultPromise success _})
      resultPromise.future
    }


    def writeBulk(address: Address, data: ByteString) = {
      val resultPromise = Promise[Unit]()
      actionsQueue.add(BulkWrite(address, data){resultPromise success _})
      resultPromise.future
    }


    override def receive = extend(super.receive) {
      case actionsQueue.FlushMsg => actionsQueue.execAll()
    }


    protected object actionsQueue {
      protected var queue = Queue.empty[Actions]
      protected var last: Option[Actions] = None

      case object FlushMsg

      def add(newActions: Actions): Unit = last match {
        case None =>
          assert(queue.isEmpty) // Sanity check
          selfRef ! FlushMsg
          log.trace(s"Adding actions ${newActions} to queue")
          last = Some(newActions)

        case Some(prevActions) => (prevActions ++ newActions) match {
          case Some(combined) =>
            log.trace(s"Combining queued actions ${prevActions} with new actions ${newActions}")
            last = Some(combined)
          case None =>
            log.trace(s"Adding actions ${newActions} to queue")
            queue = queue enqueue prevActions
            last = Some(newActions)
        }
      }

      def execAll(): Unit = {
        while (! (queue.isEmpty && last.isEmpty)) {
          if (queue.isEmpty && !last.isEmpty) {
            queue = queue enqueue last.get
            last = None
          }
          val (actions, newQueue) = queue.dequeue
          queue = newQueue
          log.trace(s"Executing queued actions ${actions}")
          actions.exec()
        }
      }
    }


    sealed trait Actions {
      def ++(that: Actions): Option[Actions]
      def exec(): Unit
    }

    case class SyncAction(reverseResultProcs: List[Unit => Unit] = Nil) extends Actions {
      def ++(actions: Actions): Option[SyncAction] = actions match {
        case that: SyncAction => Some(SyncAction(that.reverseResultProcs ++ this.reverseResultProcs))
        case _ => None
      }

      def exec() {
        import daqcore.defaults.defaultTimeout
        vmeBus.getSync().get
        registerCache = MemValues[Address, Int]()
        reverseResultProcs.reverse foreach {_({})}
      }
    }


    case class RegisterActions(
      reads: RegisterActions.Reads = RegisterActions.Reads(),
      rwWrites: RegisterActions.Writes = RegisterActions.Writes(),
      jkWrites: RegisterActions.Writes = RegisterActions.Writes()
    ) extends Actions {
      def ++(actions: Actions): Option[RegisterActions] = actions match {
        case that: RegisterActions => Some(RegisterActions(
          this.reads ++ that.reads,
          this.rwWrites ++ that.rwWrites,
          this.jkWrites ++ that.jkWrites
        ))
        case _ => None
      }

      def exec() {
        import daqcore.defaults.defaultTimeout
        val initialReadAddrs = reads.addrs filter {addr => !registerCache.contains(addr)}
        val readAddrs = rwWrites.memValues.values.foldLeft(initialReadAddrs){ case (readAddrs, (writeAddr, writeValue))=>
          if (writeValue.isMasked && !registerCache.contains(writeAddr)) readAddrs + writeAddr
          else readAddrs
        }
        if (!readAddrs.isEmpty) {
          val sortedReadAddrs = (ArrayVec.empty[Address] ++ readAddrs).sortWith(_ < _)
          val readResult = vmeBus.readIntRegs(sortedReadAddrs, regReadMode, deviceByteOrder)
          registerCache = registerCache ++ MemValues(sortedReadAddrs zip readResult.get: _*)
        }
        reads.reverseResultProcs.reverse foreach { _(registerCache) }


        val combinedWritesBuilder = ArrayVec.newBuilder[(Address,Int)]
        rwWrites.memValues.values foreach { case (addr, wr) =>
          val value = if (!wr.isMasked) wr.value else (registerCache(addr) +| wr)
          combinedWritesBuilder += ((addr, value))
        }
        jkWrites.memValues.values foreach { case (addr, wr) =>
          if (rwWrites.memValues.values.contains(addr)) throw new IllegalArgumentException("RW write and JK write to same address")
          val value = MemRegion.jkWriteValue(wr.value, wr.mask)
          combinedWritesBuilder += ((addr, value))
        }
        val combinedWrites = combinedWritesBuilder.result
        if (!combinedWrites.isEmpty) {
          registerCache = MemValues(combinedWrites.foldLeft(registerCache.values){ _ - _._1 })
          val writeResult = vmeBus.writeIntRegs(combinedWrites, regWriteMode, deviceByteOrder)
          writeResult.get
        }
        rwWrites.reverseResultProcs.reverse foreach { _(Unit) }
        jkWrites.reverseResultProcs.reverse foreach { _(Unit) }
      }
    }

    object RegisterActions {
      case class Reads(
        addrs: Set[Address] = Set.empty[Address],
        reverseResultProcs: List[(Address => Int) => Unit] = Nil
      ) {
        def++(that: Reads) = Reads(
          this.addrs ++ that.addrs,
          that.reverseResultProcs ++ this.reverseResultProcs
        )
      }

      case class Writes(
        memValues: MaskedMemValues[Address, Int] = MaskedMemValues[Address, Int](),
        reverseResultProcs: List[Unit => Unit] = Nil
      ) {
        def++(that: Writes) = Writes(
          that.memValues.values.foldLeft(this.memValues){_ + _},
          that.reverseResultProcs ++ this.reverseResultProcs
        )
      }

      def read(addr: Address)(processResult: Int => Unit): RegisterActions = RegisterActions(
        reads = Reads(
          Set(addr),
          List({ readCache: (Address => Int) => processResult(readCache(addr)) })
        )
      )

      def write(addr: Address, value: Int)(processResult: Unit => Unit): RegisterActions =
        partialRWWrite(addr, value, -1)(processResult)

      def partialRWWrite(addr: Address, value: Int, bitMask: Int)(processResult: Unit => Unit) = RegisterActions(
        rwWrites = Writes(
          MaskedMemValues(addr -> BitMaskedInteger(value, bitMask)),
          List(processResult)
        )
      )

      def partialJKWrite(addr: Address, value: Int, bitMask: Int)(processResult: Unit => Unit) = RegisterActions(
        jkWrites = Writes(
          MaskedMemValues(addr -> BitMaskedInteger(value, bitMask)),
          List(processResult)
        )
      )
    }


    case class BulkRead(address: Address, nBytes: Int)(resultProc: ByteString => Unit)  extends Actions {
      def ++(actions: Actions) = None

      def exec() = {
        import daqcore.defaults.defaultTimeout
        val result = vmeBus.readBulk(address, nBytes, bulkReadMode).get
        resultProc(result)
      }
    }


    case class BulkWrite(address: Address, data: ByteString)(resultProc: Unit => Unit) extends Actions {
      def ++(actions: Actions) = None

      def exec() = {
        import daqcore.defaults.defaultTimeout
        val result = vmeBus.writeBulk(address, data, bulkWriteMode).get
        resultProc(result)
      }
    }
  }


  object SIS3316MemoryImpl {
    protected val deviceByteOrder = BigEndian

    protected val regReadMode = VMEBus.A32_D32_SCT
    protected val regWriteMode = VMEBus.A32_D32_SCT
    protected val bulkReadMode = VMEBus.A32_D64_2eVME
    protected val bulkWriteMode = VMEBus.A32_D32_BLT
  }
}
