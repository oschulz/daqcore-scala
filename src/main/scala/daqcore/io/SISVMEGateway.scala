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
import java.util.concurrent.TimeoutException

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}
import scala.concurrent.{Future, Promise, ExecutionContext}
import scala.concurrent.duration._

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


    protected var scheduledPoll: Option[Cancellable] = None

    protected def scheduleCheckActiveRequest(): Unit = {
      if (scheduledPoll == None)
        scheduledPoll = Some(scheduleOnce(timeoutCheckInterval, selfRef, CheckActiveRequest))
    }


    override def receive = extend(super.receive) {
      case frame: ByteString if (TypedActor.context.sender == udpActorRef) =>
        addResponse(frame)
      case CheckActiveRequest =>
        scheduledPoll = None
        checkActiveRequest()
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
      addAction( UDPSyncWait(None) )
    }


    override def getSync() = {
      val result = Promise[Unit]()
      addAction( UDPSyncWait(Some(result)) )
      result.future
    }


    def readInterfaceReg(address: VMEAddress) = {
      require(isVMEInterfaceReg(address))
      val result = Promise[Int]()
      addAction( UDPVMEIfRegRead(address, result) )
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
      log.trace(s"Sending UDP request for command ${phex(cmd)} with frame ${frame map hex}")
      udp.send(frameBuilder.result())
      Thread.sleep(100)
      successful({})
    }


    def readADCRegs(addrs: Seq[VMEAddress]) = {
      require(addrs forall { ! isVMEInterfaceReg(_) })
      if (addrs.size >= 1) {
        if (addrs.size <= 64) {
            val result = Promise[ArrayVec[Int]]()
            addAction( UDPADCRegsRead(addrs, result) )
            result.future
          } else {
            concatVectors(addrs.grouped(1).toSeq map readADCRegs)(defaultExecContext)
          }
      } else {
        successful(ArrayVec.empty[Int])
      }
    }


    def writeADCRegs(addrValues: Seq[(VMEAddress, Int)]) = {
      require(addrValues forall { case(a, v) => ! isVMEInterfaceReg(a) })
      if (addrValues.size >= 1) {
        if (addrValues.size <= 64) {
            val result = Promise[Unit]()
            addAction( UDPADCRegsWrite(addrValues, result) )
            result.future
          } else {
            allComplete(addrValues.grouped(1).toSeq map writeADCRegs)(defaultExecContext)
          }
      } else {
        successful({})
      }
    }


    def readADCFifoRaw(address: VMEAddress, nBytes: Int) = {
      require (nBytes % sizeOfInt == 0)

      if (nBytes >= 1) {
        val udpRespHeaderSize = 45 // According to SIS3316 docs - why not 28 (17 + 8 + 3)?
        val maxNRespDataFrames = 15

        val maxNBytesPerReq = math.min(
          maxNRespDataFrames * (maxUDPRespPkgSize - udpRespHeaderSize),
          (maxShortValue + 1) * sizeOfInt
        ) / sizeOfInt * sizeOfInt

        if (nBytes <= maxNBytesPerReq) {
          val result = Promise[ByteString]()
          addAction( UDPADCFifoReadRaw(address, nBytes, result) )
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
          concatByteStrings(chunks map { case (a, n) => readADCFifoRaw(a, n) })(defaultExecContext)
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
          addAction( UDPADCFifoWriteRaw(address, data, result) )
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
          allComplete(chunks map { case (a, d) => writeADCFifoRaw(a, d) })(defaultExecContext)
        }
      } else {
        successful({})
      }
    }


    def resetInterface() = {
      val cmd = 0xff.toByte
      log.trace(s"Sending UDP reset command ${phex(cmd)}")
      udp.send(ByteString(cmd))
      Thread.sleep(100)
      successful({})
    }


    def readIntReg(address: VMEAddress, mode: Mode = A32_D32_SCT, deviceByteOrder: ByteOrder = BigEndian) =
      readIntRegs(Seq(address), mode, deviceByteOrder).map{ _.head }(defaultExecContext)


    def writeIntReg(address: VMEAddress, value: Int, mode: Mode = A32_D32_SCT, deviceByteOrder: ByteOrder = BigEndian) =
      writeIntRegs(Seq((address, value)), mode, deviceByteOrder)


    def readIntRegs(addrs: Seq[VMEAddress], mode: Mode, deviceByteOrder: ByteOrder) = {
      log.trace(s"readIntRegs(${loggable(addrs)}, $mode, $deviceByteOrder)")
      checkSizeMode(addrs.size * sizeOfInt, mode)
      checkDeviceByteOrder(deviceByteOrder)
      mode.cycle match {
        case VMECycle.SCT =>
          val grouped = segment(addrs.toArrayVec)(isVMEInterfaceReg)
          concatVectors (
            grouped map {
              case (true, addrSeq) => vectorize(addrSeq map readInterfaceReg)(defaultExecContext)
              case (false, addrSeq) => readADCRegs(addrSeq)
            }
          )(defaultExecContext)
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
          allComplete(
            grouped map {
              case (true, avSeq) => allComplete(avSeq map { case (a,v) => writeInterfaceReg(a, v) })(defaultExecContext)
              case (false, avSeq) => writeADCRegs(avSeq)
            }
          )(defaultExecContext)
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


    protected val reqTimeout = 50.milliseconds
    protected val timeoutCheckInterval = 10.milliseconds
    protected val maxNResend = 2
    protected val maxNReadAgain = 2

    protected var lastPkgId: Byte = -1.toByte

    protected def nextPkgId(): Byte = {
      lastPkgId = (lastPkgId + 1).toByte
      lastPkgId
    }

    protected var lastReqCmd: Byte = 0
    protected var lastReqPkgId: Byte = 0
    protected var activeUDPRequest: Option[ActiveUDPRequest] = None
    protected val waitingUDPActions = collection.mutable.Queue[UDPAction]()


    protected final class ActiveUDPRequest (val request: UDPRequest[_]) {
      var pkgIdVar: Byte = 0
      var sendTimeVar: Long = 0
      var nSentVar: Int = 0
      var nReadAgainVar: Int = 0

      def pkgId = pkgIdVar
      def sendTime = sendTimeVar
      def nSent = nSentVar
      def nReadAgain = nReadAgainVar

      def send(newPkgId: Byte): Unit = {
        pkgIdVar = newPkgId
        val frame = request.getReqFrame(pkgId)
        nSentVar = nSentVar + 1
        nReadAgainVar = 0
        if (nSent == 1) log.trace(s"Sending UDP request $request with cmd ${phex(request.cmd)}, packet id ${phex(pkgId)} and frame ${frame map hex}")
        else log.debug(s"Re-sending UDP request, cmd ${phex(request.cmd)}, packet id ${phex(pkgId)}, try no. ${nSent}")
        sendTimeVar = java.lang.System.nanoTime
        udp.send(frame)
      }

      def resend(): Unit = {
        if (nSent >= maxNResend + 1) throw new RuntimeException(s"Re-send limit reached for request $request, giving up")
        send(pkgId)
      }

      def readAgain(): Unit = {
        if (nReadAgain >= maxNReadAgain) throw new RuntimeException(s"Read-again limit reached for UDP request, packet id ${phex(pkgId)}, cmd ${phex(request.cmd)}")
        nReadAgainVar = nReadAgainVar + 1
        log.debug(s"Re-reading response to UDP request, packet id ${phex(pkgId)}, cmd ${phex(request.cmd)}, try no. ${nReadAgainVar}")
        sendTimeVar = java.lang.System.nanoTime
        udp.send(ByteString(0xEE))
      }

      def isTimedOut: Boolean = {
        val currentTime = java.lang.System.nanoTime
        val age = java.lang.System.nanoTime - sendTime
        if ((nSent > 1) || (nReadAgain > 0))
          log.debug(s"Checking active UDP request, cmd ${phex(request.cmd)}, packet id ${phex(pkgId)}, age ${age.toDouble / 1E6} ms.")
        (java.lang.System.nanoTime > sendTime + reqTimeout.toNanos)
      }
    }


    protected def setActiveRequest(activeReq: ActiveUDPRequest): Unit =  {
      require (activeUDPRequest.isEmpty)
      activeUDPRequest = Some(activeReq)
    }


    protected def removeActiveRequest(): Unit = {
      require (! activeUDPRequest.isEmpty)
      lastReqCmd = activeUDPRequest.get.request.cmd
      lastReqPkgId = activeUDPRequest.get.pkgId
      activeUDPRequest = None
      processWaitingActions()
    }


    @tailrec protected final def processWaitingActions(): Unit = {
      activeUDPRequest match {
        case Some(activeReq) =>
          log.trace(s"Waiting for active request ${activeReq.request} (packet id ${phex(activeReq.pkgId)}) to finish.")
        case None =>
          if (! waitingUDPActions.isEmpty) waitingUDPActions.dequeue match {
            case UDPSyncWait(optResult) =>
              log.trace(s"UDP sync successful")
              optResult match {
                case Some(result) => result success {}
                case None =>
              }
              processWaitingActions()
            case request: UDPRequest[_] =>
              val activeReq = new ActiveUDPRequest(request)
              setActiveRequest(activeReq)
              activeReq.send(nextPkgId)
              scheduleCheckActiveRequest()
          }
      }
    }


    protected final def checkActiveRequest(): Unit = {
      activeUDPRequest match {
        case Some(activeReq) =>
          if (activeReq.isTimedOut) {
            if (activeReq.request.canRetry) {
              activeReq.readAgain()
            } else {
              activeReq.request.result failure new TimeoutException(s"Timeout on non-retry UDP request, packet id ${phex(activeReq.pkgId)}, cmd ${phex(activeReq.request.cmd)}")
              removeActiveRequest()
            }
          } else {
            scheduleCheckActiveRequest()
          }
        case None =>
      }
    }


    protected def addAction(request: UDPAction): Unit = {
      waitingUDPActions.enqueue(request)
      processWaitingActions()
    }


    protected def addResponse(frame: ByteString): Unit = {
      val it = frame.iterator
      val cmd = it.getByte
      val pkgId = it.getByte
      activeUDPRequest match {
        case Some(activeReq) =>
          if (pkgId == activeReq.pkgId) {
            try {
              if (cmd != activeReq.request.cmd) throw new RuntimeException(s"UDP response cmd ${phex(cmd)} doesn't match request cmd ${phex(activeReq.request.cmd)}")

              if ((activeReq.nSent > 1) || (activeReq.nReadAgain > 0)) {
                log.debug(s"Received response to UDP request with packet id ${phex(pkgId)}, cmd ${phex(cmd)}, nSent = ${activeReq.nSent}, nReadAgain = ${activeReq.nReadAgain}")
              } else {
                log.trace(s"Received response to UDP request with packet id ${phex(pkgId)}")
              }

              val reqFinished = activeReq.request.processResponse(it.toByteString)

              // if ((activeReq.nSent > 1) || (activeReq.nReadAgain > 0)) {
              //   if (reqFinished) log.debug(s"Processed all responses to UDP request with packet id ${phex(pkgId)}, cmd ${phex(cmd)}, nSent = ${activeReq.nSent}, nReadAgain = ${activeReq.nReadAgain}")
              //   else log.debug(s"Processed a response to UDP request with packet id ${phex(pkgId)}, cmd ${phex(cmd)}, nSent = ${activeReq.nSent}, nReadAgain = ${activeReq.nReadAgain}")
              // }

              if (reqFinished) removeActiveRequest()
            } catch {
              case e: Exception =>
                removeActiveRequest()
                throw e
            }
          } else {
            if (activeReq.nReadAgain > 0) {
              assert(activeReq.request.canRetry)
              if ((cmd == lastReqCmd) && (pkgId == lastReqPkgId)) {
                log.debug(s"Received response to previous request (command ${phex(cmd)}, package ID ${phex(pkgId)}) on read-again, re-sending current request")
                activeReq.resend()
              } else {
                log.debug(s"Received response to unknown previous request on read-again, re-sending current request")
                activeReq.resend()
              }
            } else {
              log.debug(s"Ignoring (possibly stale) UDP response with unexpected packet ID ${phex(pkgId)}, expected ID ${phex(activeReq.pkgId)}")
            }
          }
        case None =>
          log.debug(s"Ignoring (possibly stale) UDP response with packet id ${phex(pkgId)}, no request is active")
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

    case object CheckActiveRequest


    sealed trait UDPAction


    case class UDPSyncWait(result: Option[Promise[Unit]]) extends UDPAction


    abstract class UDPRequest[T] extends UDPAction {
      implicit val nioByteOrder = gwByteOrder.nioByteOrder

      def cmd: Byte

      val timestamp: Long = System.nanoTime()

      def canRetry: Boolean

      protected def putReqContent(builder: ByteStringBuilder): Unit

      // Returns true if all responses to this request have been received:
      def processResponse(response: ByteString): Boolean

      def result: Promise[T]

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


    case class UDPVMEIfRegRead(address: VMEAddress, result: Promise[Int]) extends UDPRequest[Int] {
      def cmd = 0x10
      def canRetry = false

      protected def putReqContent(builder: ByteStringBuilder) = {
        builder.putInt(address.toUnsignedInt)
      }

      def processResponse(response: ByteString) = {
        val it = response.iterator
        val respAddr = it.getInt
        if (respAddr != address.toUnsignedInt) throw new RuntimeException(s"VME register address ${phex(respAddr)} in response to UDP command ${phex(cmd)} doesn't match requested address ${phex(address.toUnsignedInt)}")

        val data = it.toByteString
        if (data.size != sizeOfInt) throw new RuntimeException(s"Unexpected data size ${data.size} in response to UDP command ${phex(cmd)}, expected size ${sizeOfInt}")
        val dataIt = data.iterator
        result success dataIt.getInt
        true
      }
    }


    case class UDPADCRegsRead(addrs: Seq[VMEAddress], result: Promise[ArrayVec[Int]]) extends UDPRequest[ArrayVec[Int]] {
      require ((addrs.size >= 1) && (addrs.size <= 64))
      def cmd = 0x20
      def canRetry = true

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
        if (data.size != expectedDataSize) throw new RuntimeException(s"Unexpected data size ${data.size} in response to UDP command ${phex(cmd)}, expected size was ${expectedDataSize}")
        val builder = ArrayVec.newBuilder[Int]
        val dataIt = data.iterator
        for (_ <- addrs) builder += dataIt.getInt
        result success builder.result
        true
      }
    }


    case class UDPADCRegsWrite(addrValues: Seq[(VMEAddress, Int)], result: Promise[Unit]) extends UDPRequest[Unit] {
      require ((addrValues.size >= 1) && (addrValues.size <= 64))
      def cmd = 0x21
      def canRetry = true

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


    case class UDPADCFifoReadRaw(address: VMEAddress, nBytes: Int, result: Promise[ByteString]) extends UDPRequest[ByteString] {
      require ((nBytes >= 1) && (nBytes <= maxShortValue + 1))
      require(nBytes % sizeOfInt == 0)
      def cmd = 0x30
      def canRetry = false

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
        if (pkgCounter > 14) throw new RuntimeException(s"Received response packet with packet counter ${pkgCounter} to UDP command ${phex(cmd)}, packet counter should be kept <= 14 to prevent overflow")

        val data = it.toByteString
        def expectedDataSize = expectedResultSize - recvDataTotalSize
        if (data.size % sizeOfInt != 0) throw new RuntimeException(s"Unexpected data size ${data.size} in response packet ${pkgCounter} to UDP command ${phex(cmd)}, should be a multiple of ${sizeOfInt}")
        if (data.size > expectedDataSize) throw new RuntimeException(s"Unexpected data size ${data.size} in response packet ${pkgCounter} to UDP command ${phex(cmd)}, expected size was ${expectedDataSize} or less")
        if (! recvData.contains(pkgCounter)) {
          recvData.put(pkgCounter, data)
          recvDataTotalSize += data.size
        } else {
          throw new RuntimeException(s"Duplicate packet counter ${pkgCounter} in responses to UDP command ${phex(cmd)}")
        }

        assert(recvDataTotalSize <= expectedResultSize)
        if (recvDataTotalSize == expectedResultSize) {
          val resultBuilder = ByteString.newBuilder
          val sortedData = recvData.toVector sortBy { _._1 }
          var lastPkgCounter = -1
          sortedData map { case (pkgCounter, data) =>
            val expectedPkgCounter = lastPkgCounter + 1
            if (pkgCounter != expectedPkgCounter) throw new RuntimeException(s"Missing packet with counter ${expectedPkgCounter} in responses to UDP command ${phex(cmd)}")
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


    case class UDPADCFifoWriteRaw(address: VMEAddress, data: ByteString, result: Promise[Unit]) extends UDPRequest[Unit] {
      require ((data.size >= 1) && (data.size <= maxShortValue + 1))
      require(data.size % sizeOfInt == 0)
      def cmd = 0x31
      def canRetry = false

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


    def checkRespStatus(request: UDPRequest[_], status: Byte) {
      if (udpRespStatus.protError(status)) {
        throw new RuntimeException("Error response ${phex(status)} to UDP command ${phex(request.cmd)}: Protocol error")
      }
      if (udpRespStatus.accessTimeout(status)) {
        throw new RuntimeException("Error response ${phex(status)} to UDP command ${phex(request.cmd)}: Access timeout")
      }
      if (udpRespStatus.noEthGrant(status)) {
        throw new RuntimeException("Error response ${phex(status)} to UDP command ${phex(request.cmd)}: Ethernet interface has no grant")
      }
    }
  }



  protected def vectorize(futures: Seq[Future[Int]])(implicit executor: ExecutionContext): Future[ArrayVec[Int]] = {
    Future.sequence(futures) map { _.toArrayVec }
  }


  protected def concatVectors(futures: Seq[Future[ArrayVec[Int]]])(implicit executor: ExecutionContext): Future[ArrayVec[Int]] = {
    Future.sequence(futures) map { results =>
      val builder = ArrayVec.newBuilder[Int]
      results foreach { builder ++= _ }
      builder.result
    }
  }


  protected def concatByteStrings(futures: Seq[Future[ByteString]])(implicit executor: ExecutionContext): Future[ByteString] = {
    Future.sequence(futures) map { results =>
      val builder = ByteString.newBuilder
      results foreach { builder ++= _ }
      builder.result
    }
  }


  protected def allComplete(futures: TraversableOnce[Future[_]])(implicit executor: ExecutionContext): Future[Unit] = {
    Future.fold[Any, Unit](futures)({}){case (a, b) => {}}
  }
}



// trait SIS3153VMEGateway
// object SIS3153VMEGateway
