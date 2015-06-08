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


package daqcore.io

import java.net.InetSocketAddress

import scala.reflect.{ClassTag, classTag}
import scala.concurrent.{Future, Promise}

import akka.actor._

import daqcore.util._
import daqcore.io._, daqcore.io.memory._
import daqcore.actors._, daqcore.actors.TypedActorTraits._

import collection.immutable.Queue


trait SISVMEGateway extends VMEBus {
  // def udpSIS3316RegisterRead(addrs: Seq[Long]): Future[Int]
}


object SISVMEGateway extends IOResourceCompanion[SISVMEGateway] {

  def newInstance = SIS3316VMEGateway.newInstance

  abstract class GatewayImpl extends SISVMEGateway with CloseableTAImpl with SyncableImpl
}


trait SIS3316VMEGateway extends SISVMEGateway {
  import VMEBus._

  def udpClient: Future[UDPClient]

  def readInterfaceReg(address: VMEAddress): Future[Int]

  def writeInterfaceReg(address: VMEAddress, value: Int): Future[Unit]

  def readADCRegs(addrs: Seq[VMEAddress]): Future[ArrayVec[Int]]

  def writeADCRegs(addrValues: Seq[(VMEAddress, Int)]): Future[Unit]

  def readADCFifoRaw(address: VMEAddress, nBytes: Int): Future[ByteString]

  def writeADCFifoRaw(address: VMEAddress, data: ByteString): Future[Unit]

  def resetInterface(): Future[Unit]
}



object SIS3316VMEGateway extends IOResourceCompanion[SIS3316VMEGateway] {
  // Requires VME firmware version V3316-2008 or higher.

  import VMEBus._


  def newInstance = {
    case HostURL("vme-sis3316", host, port) => () => new GatewayImpl(new InetSocketAddress(host, port.getOrElse(0xE000)))
  }


  class GatewayImpl(socketAddress: InetSocketAddress) extends SISVMEGateway.GatewayImpl with SIS3316VMEGateway {
    import VMEBus._
    import GatewayImpl._

    import daqcore.defaults.defaultTimeout


    protected val udp = UDPClient(socketAddress, "udpClient")
    protected val udpActorRef = actorRef(udp)
    udp.recv(selfRef, true)

    def udpClient = successful(udp)

    protected val maxUDPReqPkgSize = 1485
    protected val maxUDPRespPkgSize = 1485

    protected def isVMEInterfaceReg(address: VMEAddress) = address < 0x20

    protected val udpActionManager = UDPActionManager(udp, log)


    override def receive = extend(super.receive) {
      case frame: ByteString if (TypedActor.context.sender == udpActorRef) =>
        udpActionManager.addResponse(frame)
    }


    protected def checkSizeMode(dataSize: Long, mode: Mode): Unit = mode match {
      case A32_D32_SCT => if (dataSize % 4 != 0) throw new IllegalArgumentException(s"Invalid data size $dataSize for VME D32")
      case A32_D32_BLT => if (dataSize % 4 != 0) throw new IllegalArgumentException(s"Invalid data size $dataSize for VME D32")
      case A32_D64_MBLT => if (dataSize % 8 != 0) throw new IllegalArgumentException(s"Invalid data size $dataSize for VME D64")
      case A32_D64_2eVME => if (dataSize % 8 != 0) throw new IllegalArgumentException(s"Invalid data size $dataSize for VME D64")
      case _ => throw new UnsupportedOperationException(s"VME mode $mode not supported")
    }


    def baseAddress = successful(0)


    protected def checkDeviceByteOrder(deviceByteOrder: ByteOrder) {
      if (deviceByteOrder != BigEndian)
        throw new UnsupportedOperationException(s"Device byte order $deviceByteOrder not supported.")
    }

    protected def dataEncoding(mode: Mode, deviceByteOrder: ByteOrder) = {
      // Trivial implementation for pure 32-bit big-endian VME devices like the SIS3316.
      // Support for other devices would require a more complex implementation.
      checkDeviceByteOrder(deviceByteOrder)
      gwByteOrder
    }

    def bulkEncoding(mode: Mode, deviceByteOrder: ByteOrder) =
      successful(dataEncoding(mode, deviceByteOrder))


    override def sync() = {
      udpActionManager.addRequest( UDPSyncWait(None) )
    }


    override def getSync() = {
      val result = Promise[Unit]()
      udpActionManager.addRequest( UDPSyncWait(Some(result)) )
      result.future
    }


    def readInterfaceReg(address: VMEAddress) = {
      require(isVMEInterfaceReg(address))
      val result = Promise[Int]()
      udpActionManager.addRequest( UDPVMEIfRegRead(address, result) )
      result.future
    }


    def writeInterfaceReg(address: VMEAddress, value: Int) = {
      require(isVMEInterfaceReg(address))
      implicit val nioByteOrder = gwByteOrder.nioByteOrder
      val frameBuilder = ByteString.newBuilder
      frameBuilder.sizeHint(sizeOfByte + 2 * sizeOfInt)
      val cmd = 0x11.toByte
      frameBuilder.putByte(cmd)
      frameBuilder.putInt(address.toUnsignedInt)
      frameBuilder.putInt(value)
      val frame = frameBuilder.result()
      log.trace(s"Sending UDP request for command 0x${hex(cmd)} with frame ${frame map hex}")
      udp.send(frameBuilder.result())
      Thread.sleep(100)
      successful({})
    }


    def readADCRegs(addrs: Seq[VMEAddress]) = {
      require(addrs forall { ! isVMEInterfaceReg(_) })
      if (addrs.size >= 1) {
        if (addrs.size <= 1) {
            // Should be able to read 64 addresses at once, but doesn't seem to work correctly
            val result = Promise[ArrayVec[Int]]()
            udpActionManager.addRequest( UDPADCRegsRead(addrs, result) )
            result.future
          } else {
            Future.sequence(addrs.grouped(1).toSeq map readADCRegs) map { results =>
              val builder = ArrayVec.newBuilder[Int]
              results foreach { builder ++= _ }
              builder.result
            }
          }
      } else {
        successful(ArrayVec.empty[Int])
      }
    }


    def writeADCRegs(addrValues: Seq[(VMEAddress, Int)]) = {
      require(addrValues forall { case(a, v) => ! isVMEInterfaceReg(a) })
      if (addrValues.size >= 1) {
        if (addrValues.size <= 1) {
            // Should be able to write 64 addresses at once, but doesn't seem to work correctly
            val result = Promise[Unit]()
            udpActionManager.addRequest( UDPADCRegsWrite(addrValues, result) )
            result.future
          } else {
            Future.sequence(addrValues.grouped(1).toSeq map writeADCRegs) map
              { results => {} }
          }
      } else {
        successful({})
      }
    }


    def readADCFifoRaw(address: VMEAddress, nBytes: Int) = {
      require ((nBytes > 0) && (nBytes % sizeOfInt == 0))

      val udpRespHeaderSize = 45 // According to SIS3316 docs - why not 28 (17 + 8 + 3)?
      val maxNRespDataFrames = 15

      val maxNBytesPerReq = math.min(
        maxNRespDataFrames * (maxUDPRespPkgSize - udpRespHeaderSize),
        (maxShortValue + 1) * sizeOfInt
      ) / sizeOfInt * sizeOfInt

      if (nBytes >= 1) {
        if (nBytes <= maxNBytesPerReq) {
          val result = Promise[ByteString]()
          udpActionManager.addRequest( UDPADCFifoReadRaw(address, nBytes, result) )
          result.future
        } else {
          val chunks = {
            val builder = Seq.newBuilder[(VMEAddress, Int)]

            // With the SIS3316 UDP Interface, address does not increase from
            // chunk to chunk for a FIFO read (different from SIS3153):
            val reqAddr = address

            var rest = nBytes
            while (rest > 0) {
              val reqNBytes = if (rest > maxNBytesPerReq) maxNBytesPerReq else rest
              builder += ((reqAddr, reqNBytes))
              rest -= reqNBytes
            }
            assert(rest == 0)
            builder.result
          }
          log.trace(s"chunks = $chunks")
          Future.sequence(chunks map { case (a, n) => readADCFifoRaw(a, n) }) map { results =>
            val builder = ByteString.newBuilder
            results foreach { builder ++= _ }
            builder.result
          }
        }
      } else {
        successful(ByteString())
      }
    }


    def writeADCFifoRaw(address: VMEAddress, data: ByteString) = {
      require ((data.size > 0) && (data.size % sizeOfInt == 0))

      val udpReqHeaderSize = 45 // Just to be on the safe side

      val maxNBytesPerReq = math.min(
        maxUDPReqPkgSize - udpReqHeaderSize,
        (maxShortValue + 1) * sizeOfInt
      ) / sizeOfInt * sizeOfInt

      if (data.size >= 1) {
        if (data.size <= maxNBytesPerReq) {
          val result = Promise[Unit]()
          udpActionManager.addRequest( UDPADCFifoWriteRaw(address, data, result) )
          result.future
        } else {
          val chunks = {
            val builder = Seq.newBuilder[(VMEAddress, ByteString)]
            var reqAddr = address
            var rest = data
            while (! rest.isEmpty) {
              val reqNBytes = if (rest.size > maxNBytesPerReq) maxNBytesPerReq else rest.size
              builder += ((reqAddr, rest.take(reqNBytes)))
              reqAddr += reqNBytes
              rest = rest.drop(reqNBytes)
            }
            builder.result
          }
          Future.sequence(chunks map { case (a, d) => writeADCFifoRaw(a, d) }) map
            { results => {} }
        }
      } else {
        successful({})
      }
    }


    def resetInterface() = {
      val cmd = 0xff.toByte
      log.trace(s"Sending UDP reset command 0x${hex(cmd)}")
      udp.send(ByteString(cmd))
      Thread.sleep(100)
      successful({})
    }


    def readIntReg(address: VMEAddress, mode: Mode = A32_D32_SCT, deviceByteOrder: ByteOrder = BigEndian) =
      readIntRegs(Seq(address), mode, deviceByteOrder) map { _.head }


    def writeIntReg(address: VMEAddress, value: Int, mode: Mode = A32_D32_SCT, deviceByteOrder: ByteOrder = BigEndian) =
      writeIntRegs(Seq((address, value)), mode, deviceByteOrder)


    def readIntRegs(addrs: Seq[VMEAddress], mode: Mode, deviceByteOrder: ByteOrder) = {
      log.trace(s"readIntRegs(${loggable(addrs)}, $mode, $deviceByteOrder)")
      checkSizeMode(addrs.size * sizeOfInt, mode)
      checkDeviceByteOrder(deviceByteOrder)
      mode.cycle match {
        case VMECycle.SCT =>
          val grouped = segment(addrs.toArrayVec)(isVMEInterfaceReg)
          Future.sequence (
            grouped map {
              case (true, addrSeq) => Future.sequence(addrSeq map readInterfaceReg)
              case (false, addrSeq) => readADCRegs(addrSeq)
            }
          ) map { _.flattenToArrayVec }
        case _ =>
          throw new UnsupportedOperationException("VME cycle ${mode.cycle} not supported for reading of individual registers")
      }
    }


    def writeIntRegs(addrValues: Seq[(VMEAddress, Int)], mode: Mode, deviceByteOrder: ByteOrder) = {
      log.trace(s"writeIntRegs(${loggable(addrValues)}, $mode, $deviceByteOrder)")
      checkSizeMode(addrValues.size * sizeOfInt, mode)
      checkDeviceByteOrder(deviceByteOrder)
      mode.cycle match {
        case VMECycle.SCT =>
          val grouped = segment(addrValues.toArrayVec) { case(a, v) => isVMEInterfaceReg(a) }
          Future.sequence {
            grouped map {
              case (true, avSeq) => Future.sequence(avSeq map { case (a,v) => writeInterfaceReg(a, v) })
              case (false, avSeq) => writeADCRegs(avSeq)
            }
          } map { _ => {} }
        case _ =>
          throw new UnsupportedOperationException("VME cycle ${mode.cycle} not supported for reading of individual registers")
      }
    }


    def readBulk(address: VMEAddress, nBytes: Int, mode: Mode) = {
      log.trace(s"readBulk($address, $nBytes, $mode)")
      checkSizeMode(nBytes, mode)
      mode.cycle match {
        case VMECycle.SCT =>
          throw new UnsupportedOperationException("VME cycle ${mode.cycle} not supported for bulk write (yet)")
        case _ =>
          readADCFifoRaw(address, nBytes)
      }
    }


    def writeBulk(address: VMEAddress, data: ByteString, mode: Mode) = {
      log.trace(s"writeBulk($address, ${loggable(data)}, $mode)")
      checkSizeMode(data.size, mode)
      mode.cycle match {
        case VMECycle.SCT =>
          throw new UnsupportedOperationException("VME cycle ${mode.cycle} not supported for bulk write (yet)")
        case _ =>
          writeADCFifoRaw(address, data)
      }
    }
  }


  object GatewayImpl {
    import VMEBus._

    val gwByteOrder = LittleEndian
    val sizeOfByte = implicitly[IntegerNumType[Byte]].nBytes
    val sizeOfShort = implicitly[IntegerNumType[Short]].nBytes
    val sizeOfInt = implicitly[IntegerNumType[Int]].nBytes

    val maxShortValue = implicitly[IntegerNumType[Short]].unsignedMax.asUnsigned

    def segment[@specialized(Byte, Short, Int, Long) A: ClassTag](xs: ArrayVec[A])(condition: A => Boolean): Seq[(Boolean, ArrayVec[A])] = {
      val builder = List.newBuilder[(Boolean, ArrayVec[A])]
      var rest = xs.iterator.buffered
      while (! rest.isEmpty) {
        val headCondition = condition(rest.head)
        val (a, b) = rest span { x => condition(x) == headCondition }
        builder += ((headCondition, a.toArrayVec))
        rest = b
      }
      builder.result
    }


    sealed trait UDPAction


    case class UDPSyncWait(result: Option[Promise[Unit]]) extends UDPAction


    abstract class UDPRequest extends UDPAction {
      implicit val nioByteOrder = gwByteOrder.nioByteOrder

      def cmd: Byte

      val timestamp: Long = System.nanoTime()

      def canBePipelined: Boolean = true

      protected def putReqContent(builder: ByteStringBuilder): Unit

      // Returns true if all responses to this request have been received:
      def processResponse(response: ByteString): Boolean

      def getReqFrame(pkgId: Byte): ByteString = {
        val frameBuilder = ByteString.newBuilder
        putReqFrame(frameBuilder, pkgId)
        frameBuilder.result()
      }

      def putReqFrame(builder: ByteStringBuilder, pkgId: Byte) = {
        builder.putByte(cmd)
        builder.putByte(pkgId)
        putReqContent(builder)
      }
    }


    case class UDPActionManager(udp: UDPClient, log: Logging.Logger) {
      protected var nextPkgId: Byte = 0
      protected val defaultMaxActiveUDPRequests = 1  // Pipelined requests don't seem to work right yet
      protected var currentMaxActiveUDPRequests = defaultMaxActiveUDPRequests
      protected val activeUDPRequests = collection.mutable.HashMap[Byte, UDPRequest]()
      protected val waitingUDPActions = collection.mutable.Queue[UDPAction]()

      protected def canExecute(request: UDPRequest): Boolean = {
        if (activeUDPRequests.isEmpty) {
          true
        } else if (request.canBePipelined) {
          if (activeUDPRequests.size < currentMaxActiveUDPRequests) {
            true
          } else {
            log.trace(s"Can't send UDP request yet, ${activeUDPRequests.size} of max. ${currentMaxActiveUDPRequests} pending") 
            false
          }
        } else {
          log.trace(s"Can't send UDP request yet, request can't be pipelined and ${activeUDPRequests.size} requests pending")
          false
        }
      }


      protected def execWaitingUDPActions(): Unit = if (! waitingUDPActions.isEmpty) {
        log.trace(s"Trying to execute ${waitingUDPActions.size} waiting UDP actions, ${activeUDPRequests.size} requests pending")
        var continue = true
        while ( (! waitingUDPActions.isEmpty) && continue ) {
          waitingUDPActions.head match {
            case UDPSyncWait(optResult) =>
              if (activeUDPRequests.isEmpty) {
                  log.trace(s"UDP sync successful (no active requests).")
                  waitingUDPActions.dequeue
                  optResult match {
                    case Some(result) => result success {}
                    case None =>
                  }
                } else {
                  log.trace(s"UDP sync requested, waiting for ${activeUDPRequests.size} active requests to finish.")
                  continue = false
                }
            case request: UDPRequest =>
              if (canExecute(request)) {
                waitingUDPActions.dequeue
                val pkgId = nextPkgId
                if (! activeUDPRequests.contains(pkgId)) {
                  nextPkgId = (nextPkgId + 1).toByte
                  val frame = request.getReqFrame(pkgId)
                  log.trace(s"Sending waiting UDP request $request with packet id 0x${hex(pkgId)} and frame ${frame map hex}")
                  udp.send(frame)
                  activeUDPRequests.put(pkgId, request)
                  if (! request.canBePipelined) currentMaxActiveUDPRequests = 1
                } else {
                  log.trace(s"Blocked by pending request with packet id 0x${hex(pkgId)} (packet loss?)")
                  continue = false
                }
              } else {
                continue = false
              }
          }
        }
      }

      def addRequest(request: UDPAction): Unit = {
        waitingUDPActions.enqueue(request)
        execWaitingUDPActions()
      }

      def removeActiveRequest(pkgId: Byte): Unit = {
        activeUDPRequests.remove(pkgId)
        if (activeUDPRequests.isEmpty) currentMaxActiveUDPRequests = defaultMaxActiveUDPRequests
      }

      def addResponse(frame: ByteString): Unit = {
        val it = frame.iterator
        val cmd = it.getByte
        val pkgId = it.getByte
        activeUDPRequests.get(pkgId) match {
          case Some(request) =>
            log.trace(s"Received response to UDP request with packet id 0x${hex(pkgId)}")
            try {
              if (cmd != request.cmd) throw new RuntimeException(s"UDP response cmd code 0x${hex(cmd)} doesn't mach request cmd code 0x${hex(request.cmd)}")
              val reqFinished = request.processResponse(it.toByteString)
              if (reqFinished) removeActiveRequest(pkgId)
            } catch {
              case e: Exception =>
                removeActiveRequest(pkgId)
                throw e
            }
          case None =>
            throw new RuntimeException(s"Received UDP response with unexpected packet id 0x${hex(pkgId)}")
        }
        execWaitingUDPActions()
      }
    }


    case class UDPVMEIfRegRead(address: VMEAddress, result: Promise[Int]) extends UDPRequest {
      def cmd = 0x10

      override def canBePipelined: Boolean = false

      protected def putReqContent(builder: ByteStringBuilder) = {
        builder.putInt(address.toUnsignedInt)
      }

      def processResponse(response: ByteString) = {
        val it = response.iterator
        val respAddr = it.getInt
        if (respAddr != address.toUnsignedInt) throw new RuntimeException(s"VME register address 0x${hex(respAddr)} in response to UDP command 0x${hex(cmd)} doesn't match requested address 0x${hex(address.toUnsignedInt)}")

        val data = it.toByteString
        if (data.size != sizeOfInt) throw new RuntimeException(s"Unexpected data size ${data.size} in response to UDP command 0x${hex(cmd)}, expected size ${sizeOfInt}")
        val dataIt = data.iterator
        result success dataIt.getInt
        true
      }
    }


    case class UDPADCRegsRead(addrs: Seq[VMEAddress], result: Promise[ArrayVec[Int]]) extends UDPRequest {
      require ((addrs.size >= 1) && (addrs.size <= 64))
      def cmd = 0x20

      protected def putReqContent(builder: ByteStringBuilder) = {
        builder.sizeHint(sizeOfShort + addrs.size * sizeOfInt)
        builder.putShort((addrs.size - 1).toUnsignedShort)
        for (address <- addrs) builder.putInt(address.toUnsignedInt)
      }

      def processResponse(response: ByteString) = {
        val it = response.iterator
        val status = it.getByte
        checkRespStatus(this, status)

        val data = it.toByteString
        val expectedDataSize = addrs.size * sizeOfInt
        if (data.size != expectedDataSize) throw new RuntimeException(s"Unexpected data size ${data.size} in response to UDP command 0x${hex(cmd)}, expected size was ${expectedDataSize}")
        val builder = ArrayVec.newBuilder[Int]
        val dataIt = data.iterator
        for (_ <- addrs) builder += dataIt.getInt
        result success builder.result
        true
      }
    }


    case class UDPADCRegsWrite(addrValues: Seq[(VMEAddress, Int)], result: Promise[Unit]) extends UDPRequest {
      require ((addrValues.size >= 1) && (addrValues.size <= 64))
      def cmd = 0x21

      protected def putReqContent(builder: ByteStringBuilder) = {
        builder.putShort((addrValues.size - 1).toUnsignedShort)
        builder.sizeHint(2 * addrValues.size * sizeOfInt)
        addrValues map { case (address, value) =>
          builder.putInt(address.toUnsignedInt)
          builder.putInt(value)
        }
      }

      def processResponse(response: ByteString) = {
        val status = response.iterator.getByte
        checkRespStatus(this, status)
        result success {}
        true
      }
    }


    case class UDPADCFifoReadRaw(address: VMEAddress, nBytes: Int, result: Promise[ByteString]) extends UDPRequest {
      require ((nBytes >= 1) && (nBytes <= maxShortValue + 1))
      require(nBytes % sizeOfInt == 0)
      def cmd = 0x30

      override def canBePipelined: Boolean = false

      def expectedResultSize = nBytes
      protected val recvData = collection.mutable.HashMap[Int, ByteString]()
      var recvDataTotalSize: Int = 0

      protected def putReqContent(builder: ByteStringBuilder) = {
        builder.sizeHint(sizeOfShort + sizeOfInt)
        builder.putShort((nBytes / sizeOfInt - 1).toUnsignedShort)
        builder.putInt(address.toUnsignedInt)
      }

      def processResponse(response: ByteString) = {
        val it = response.iterator
        val status = it.getByte
        checkRespStatus(this, status)
        val pkgCounter = udpRespStatus.pkgCounter(status)
        if (pkgCounter > 14) throw new RuntimeException(s"Received response packet with packet counter ${pkgCounter} to UDP command 0x${hex(cmd)}, packet counter should be kept <= 14 to prevent overflow")

        val data = it.toByteString
        def expectedDataSize = expectedResultSize - recvDataTotalSize
        if (data.size % sizeOfInt != 0) throw new RuntimeException(s"Unexpected data size ${data.size} in response packet ${pkgCounter} to UDP command 0x${hex(cmd)}, should be a multiple of ${sizeOfInt}")
        if (data.size > expectedDataSize) throw new RuntimeException(s"Unexpected data size ${data.size} in response packet ${pkgCounter} to UDP command 0x${hex(cmd)}, expected size was ${expectedDataSize} or less")
        if (! recvData.contains(pkgCounter)) {
          recvData.put(pkgCounter, data)
          recvDataTotalSize += data.size
        } else {
          throw new RuntimeException(s"Duplicate packet counter ${pkgCounter} in responses to UDP command 0x${hex(cmd)}")
        }

        assert(recvDataTotalSize <= expectedResultSize)
        if (recvDataTotalSize == expectedResultSize) {
          val resultBuilder = ByteString.newBuilder
          val sortedData = recvData.toVector sortBy { _._1 }
          var lastPkgCounter = -1
          sortedData map { case (pkgCounter, data) =>
            val expectedPkgCounter = lastPkgCounter + 1
            if (pkgCounter != expectedPkgCounter) throw new RuntimeException(s"Missing packet with counter ${expectedPkgCounter} in responses to UDP command 0x${hex(cmd)}")
            resultBuilder ++= data
            lastPkgCounter = pkgCounter
          }
          val resultData = resultBuilder.result
          assert (resultData.size == expectedResultSize)
          result success resultData
          true
        } else {
          false
        }
      }
    }


    case class UDPADCFifoWriteRaw(address: VMEAddress, data: ByteString, result: Promise[Unit]) extends UDPRequest {
      require ((data.size >= 1) && (data.size <= maxShortValue + 1))
      require(data.size % sizeOfInt == 0)
      def cmd = 0x31

      override def canBePipelined: Boolean = false

      protected def putReqContent(builder: ByteStringBuilder) = {
        builder.sizeHint(sizeOfShort + sizeOfInt + data.size)
        builder.putShort((data.size / sizeOfInt - 1).toUnsignedShort)
        builder.putInt(address.toUnsignedInt)
        builder ++= data
      }

      def processResponse(response: ByteString) = {
        val it = response.iterator
        val status = it.getByte
        checkRespStatus(this, status)
        true
      }
    }


    class UDPRespStatus extends SimpleRegister[Int] {
      val reqCounter = RegBit(7)  // Request counter, toggles with each request
      val protError = RegBit(6)  // Protocol error (request command packet error)
      val accessTimeout = RegBit(5)  // SIS3316 access timeout (fifo empty)
      val noEthGrant = RegBit(4)  // Ethernet interface has no grant
      val pkgCounter = RegBits(0 to 3)  // Packet counter
    }
    val udpRespStatus = new UDPRespStatus


    def checkRespStatus(request: UDPRequest, status: Byte) {
      if (udpRespStatus.protError(status)) {
        throw new RuntimeException("Error response 0x${hex(status)} to UDP command 0x${hex(request.cmd)}: Protocol error")
      }
      if (udpRespStatus.accessTimeout(status)) {
        throw new RuntimeException("Error response 0x${hex(status)} to UDP command 0x${hex(request.cmd)}: Access timeout")
      }
      if (udpRespStatus.noEthGrant(status)) {
        throw new RuntimeException("Error response 0x${hex(status)} to UDP command 0x${hex(request.cmd)}: Ethernet interface has no grant")
      }
    }
  }

}



// trait SIS3153VMEGateway
// object SIS3153VMEGateway
