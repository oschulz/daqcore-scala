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

import scala.collection.immutable.{IntMap, SortedMap}
import scala.collection.MapLike
import scala.actors._

import daqcore.util._
import daqcore.actors._
import daqcore.profiles._
import daqcore.monads._


class SIS3300(val vmeBus: VMEBus, val baseAddress: Int) extends Server {
  import SIS3300._

  override def init() = {
  }


  def serve = {
    case _ =>
  }

  
  val memory = new SIS3300.SISMemory(vmeBus, baseAddress)
  import memory._

  case class Settings (
    daq: DAQSettings = DAQSettings(),
    trigger: TriggerSettings = TriggerSettings (
      thresholds = SortedMap.empty[Int, TriggerThreshold],
      mode = MNPTriggerMode()
    )
  )
  
  var settingsVar: Settings = Settings()
  def settings = settingsVar

  def get(reg: ReadableRegister): Int = {
    run { for {
      contents <- reg get()
      _ <- sync()
    } yield {contents()} }
  }

  def get(bits: ReadableRegister#ReadableBits): Int = {
    run { for {
      contents <- bits get()
      _ <- sync()
    } yield {contents()} }
  }
  
  
  def channels = (1 to 9)

  protected var currentBankVar = 1
  def currentBank = currentBankVar

  def resetModule(): Unit = {
    run { for {
      _ <- KEY_RESET set()
      _ <- sync()
    } yield {} }
    
    currentBankVar = 1
  }

  def initModule(): Unit = {
    run { for {
      _ <- CONTROL_STATUS.USRTRGOUT set 1
      _ <- CONTROL_STATUS.TRGARMST set 1
      _ <- CONTROL_STATUS.TRGROUTE set 1
      _ <- CONTROL_STATUS.BKFULLOUT2 set 1
      _ <- sync()
    } yield {} }
    
    val info = getInfo()
    debug("Module initialized, module info: " + info)
  }
  
  
  def setUserLED(state: Boolean): Unit = {
    run { for {
      _ <- CONTROL_STATUS.USRLED set (if (state) 1 else 0)
      _ <- sync()
    } yield {} }
  }


  def getUserLED(): Boolean = {
    run { for {
      state <- CONTROL_STATUS.USRLED get()
      _ <- sync()
    } yield {
      if (state() == 1) true else false
    } }
  }

  
  def getInfo(): ModuleInfo = {
    run { for { 
      majRev <- MODID.MAJREV get()
      minRev <- MODID.MINREV get()
      modID <- MODID.MODID get()
      _ <- sync()
    } yield {
      ModuleInfo(hex(modID()), hex(majRev())+hex(minRev()))
    } }
  }


  def setupIRQ(level: Int, vector: Int): Unit = {
    require (((0 to 0x8) contains level) && ((0 to 0x100) contains vector))
    //!! setup IRQ on vme link ...

    run { for {
      _ <- IRQ_CONFIG.VECTOR set vector
      _ <- IRQ_CONFIG.LEVEL set level
      // _ <- IRQ_CONFIG.IRQEN set 1
      _ <- IRQ_CONFIG.ROAK set 1

      _ <- IRQ_CONTROL.SRC0EN set 0
      _ <- IRQ_CONTROL.SRC1EN set 1
      _ <- IRQ_CONTROL.SRC2EN set 0
      _ <- IRQ_CONTROL.SRC3EN set 0
      _ <- sync()
    } yield {} }
  }


  def clearIRQ(): Unit = {
    run { for {
      _ <- IRQ_CONTROL.SRC1CLR clear()
      _ <- sync()
    } yield {} }
  }


  def getIRQStatus: Boolean = {
    run { for {
      isSet <- IRQ_CONTROL.SRC1IS get()
      _ <- sync()
    } yield { isSet() == 1; } }
  }

  
  def set(toSet: DAQSettings): Unit = {
    val clock = 1E9.toLong
    
    val modNSampes = findNearestInt(pageConfigTable.keys, toSet.nSamples)
    val modStopDelay = findNearestInt((0 to 0x10000), toSet.stopDelay)
    val modNAverage = findNearestInt(avgConfigTable.keys, toSet.nAverage)
    val modSampleRate = findNearest(clockSourceTable.keys, toSet.sampleRate)
    val tsPreDiv = findNearest((0x1 to 0x10000), toSet.tsBase * clock)
    val maxNPages = pageConfigTable(modNSampes).nEvents
    val modNPages = findNearestInt((1 to maxNPages), toSet.nPages)

    val clampedSettings = DAQSettings (
      nSamples = modNSampes,
      stopDelay = modStopDelay,
      nAverage = modNAverage,
      sampleRate = modSampleRate,
      tsBase = clock / tsPreDiv,
      nPages = modNPages
    )

    debug("Setting: " + clampedSettings)

    run { for {
      _ <- ACQUISITION_CONTROL.CLOCKSRC set clockSourceTable(modSampleRate)
      _ <- ACQUISITION_CONTROL.STOPDELAY_EN set 1
      _ <- STOP_DELAY.STOPDEL set modStopDelay
      _ <- TIMESTAMP_PREDIVIDER.TSPREDEV set tsPreDiv
      _ <- sync()
    } yield {} }
    
    for (reg <- Seq(EVENT_CONFIG_ADC12, EVENT_CONFIG_ADC34, EVENT_CONFIG_ADC56, EVENT_CONFIG_ADC78)) {
      run { for {
        _ <- reg.PGSIZE set pageConfigTable(modNSampes).pgs
        _ <- reg.PGSIZEMAPSEL set pageConfigTable(modNSampes).psm
        _ <- reg.AVERAGE set avgConfigTable(modNAverage)
        _ <- reg.WRAP set 1
        _ <- sync()
      } yield {} }
    }
    
    for (reg <- Seq(MAX_NO_OF_EVENTS_ADC12, MAX_NO_OF_EVENTS_ADC34, MAX_NO_OF_EVENTS_ADC56, MAX_NO_OF_EVENTS_ADC78)) {
      run { for {
        _ <- reg.MAXNEV set modNPages
        _ <- sync()
      } yield {} }
    }
    
    settingsVar = settingsVar.copy(daq = clampedSettings)
  }
  
  
  def set(thresholds: (Int, TriggerThreshold)*) {
    val threshMap =
      SortedMap(channels map {ch => ch -> TrigOff}: _*) ++
        (settings.trigger.thresholds) ++
        SortedMap(thresholds: _*)
    
    val cfg = threshMap map { e =>
      val (ch, TriggerThreshold(threshold, pol)) = e
      ch -> TriggerThreshold(
        findNearestInt(sampleRange, threshold),
        pol
      )
    }
    
    debug("Setting: " + cfg)

    run { for {
      _ <- TRIGGER_THRESHOLD_ADC12.THRESHODD set cfg(1).threshold
      _ <- TRIGGER_THRESHOLD_ADC12.GTLEODD set (if (cfg(1).polarity) 0 else 1)
      _ <- TRIGGER_THRESHOLD_ADC12.THRESHEVEN set cfg(2).threshold
      _ <- TRIGGER_THRESHOLD_ADC12.GTLEEVEN set (if (cfg(2).polarity) 0 else 1)

      _ <- TRIGGER_THRESHOLD_ADC34.THRESHODD set cfg(3).threshold
      _ <- TRIGGER_THRESHOLD_ADC34.GTLEODD set (if (cfg(3).polarity) 0 else 1)
      _ <- TRIGGER_THRESHOLD_ADC34.THRESHEVEN set cfg(4).threshold
      _ <- TRIGGER_THRESHOLD_ADC34.GTLEEVEN set (if (cfg(4).polarity) 0 else 1)

      _ <- TRIGGER_THRESHOLD_ADC56.THRESHODD set cfg(5).threshold
      _ <- TRIGGER_THRESHOLD_ADC56.GTLEODD set (if (cfg(5).polarity) 0 else 1)
      _ <- TRIGGER_THRESHOLD_ADC56.THRESHEVEN set cfg(6).threshold
      _ <- TRIGGER_THRESHOLD_ADC56.GTLEEVEN set (if (cfg(6).polarity) 0 else 1)

      _ <- TRIGGER_THRESHOLD_ADC78.THRESHODD set cfg(7).threshold
      _ <- TRIGGER_THRESHOLD_ADC78.GTLEODD set (if (cfg(7).polarity) 0 else 1)
      _ <- TRIGGER_THRESHOLD_ADC78.THRESHEVEN set cfg(8).threshold
      _ <- TRIGGER_THRESHOLD_ADC78.GTLEEVEN set (if (cfg(8).polarity) 0 else 1)
      _ <- sync()
    } yield {} }
    
    settingsVar = settingsVar.copy (
      trigger = settingsVar.trigger.copy (
        thresholds = cfg
      )
    )
  }


  def set(toSet: MNPTriggerMode) {
    val clampedSettings = {
      import toSet._
      MNPTriggerMode (
        m = findNearestInt((0 to 16), m),
        n = findNearestInt((0 to 16), n),
        p = findNearestInt((0 to 16), p)
      )
    }

    debug("Setting: " + clampedSettings)
  
    for (reg <- Seq(TRIGGER_SETUP_ADC12, TRIGGER_SETUP_ADC34, TRIGGER_SETUP_ADC56, TRIGGER_SETUP_ADC78)) {
      import clampedSettings._
      run { for {
        _ <- reg.M set m
        _ <- reg.N set n
        _ <- reg.P set p
        _ <- reg.NM set 1
        _ <- reg.PULSE set (if (p > 0) 1 else 0)
        _ <- sync()
      } yield {} }
    }

    settingsVar = settingsVar.copy (
      trigger = settingsVar.trigger.copy (
        mode = clampedSettings
      )
    )
  }


  def startCapture(): Unit = {
    run { for {
      _ <- ACQUISITION_CONTROL.AUTOSTART_EN set 1
      _ <- ACQUISITION_CONTROL.MULTIEVENT_EN set 1
      _ <- ACQUISITION_CONTROL.SCLOCKB1_EN set 1
      _ <- ACQUISITION_CONTROL.AUTOBANK_EN set 1
      _ <- sync()
      _ <- KEY_START_AUTO_BANK_SWITCH set()
      _ <- sync()
    } yield {} }
  }

  def stopCapture(): Unit = {
    run { for {
      _ <- KEY_STOP_AUTO_BANK_SWITCH set()
      _ <- sync()
      _ <- ACQUISITION_CONTROL.AUTOSTART_EN set 0
      _ <- ACQUISITION_CONTROL.SCLOCKB1_EN set 0
      _ <- ACQUISITION_CONTROL.AUTOBANK_EN set 0
      _ <- sync()
    } yield {} }
  }

  def getBankBusy(): Boolean = {
    val bit = if (currentBank == 1) ACQUISITION_CONTROL.BANK1_BUSY else ACQUISITION_CONTROL.BANK2_BUSY
    run { for {
      v <- bit get()
      _ <- sync()
    } yield {
      if (v() == 1) true else false
    } }
  }

  def getBankFull(): Boolean = {
    val bit = if (currentBank == 1) ACQUISITION_CONTROL.BANK1_FULL else ACQUISITION_CONTROL.BANK2_FULL
    run { for {
      v <- bit get()
      _ <- sync()
    } yield {
      if (v() == 1) true else false
    } }
  }


  def getNEvents(): Int = {
      val bits = if (currentBank == 1) BANK1_EVENT_CNT_ADC12.NEVENTS else BANK2_EVENT_CNT_ADC12.NEVENTS
      run { for {
        v <- bits get()
        _ <- sync()
      } yield {
        v()
      } }
  }


  def clearBankFull(): Unit = {
      val reg = if (currentBankVar == 1) KEY_BANK1_FULL_FLAG else KEY_BANK2_FULL_FLAG
      run { for {
        _ <- reg set()
        _ <- sync()
      } yield {} }
      currentBankVar = if (currentBankVar ==1) 2 else 1
  }

}



object SIS3300 extends Logging {
  case class ModuleInfo(modID: String, revision: String)

  case class DAQSettings (
    nSamples: Int = 4096,
    stopDelay: Int = 2048,
    nAverage: Int = 1,
    sampleRate: Double = 100E6,
    tsBase: Double = 10E-9.toLong, // in ns
    nPages: Int = Int.MaxValue
  )

  case class TriggerSettings (
    thresholds: SortedMap[Int, TriggerThreshold],
    mode: TriggerMode
  )
  
  case class TriggerThreshold (
    threshold: Int = 0,
    polarity: Boolean = true
  )
  
  abstract class TriggerMode
  case class MNPTriggerMode (m: Int = 0x8, n: Int = 0x8, p: Int = 0x8) extends TriggerMode
  

  import math.{max,min,abs,log}

  type Address = Int
  type Word = Int
  val Word = Int
  type MemMap = IntMap[Word]
  def EmptyMemMap = IntMap.empty[Word]
  def WordFullBitmask = (Word.MaxValue+Word.MinValue)
  val nWordBits = (8 * sizeOf[Word])
  
  object RegisterRange { def apply(start: Address, end: Address) = Range(start, end, sizeOf[Word]) }


  def findNearest(set: Iterable[Int], v: Double): Int =
    set.foldLeft(set.head) { (r, e) => if (abs(v-e.toDouble) < abs(v-r.toDouble)) e else r }

  def findNearest(set: Iterable[Long], v: Double): Long =
    set.foldLeft(set.head) { (r, e) => if (abs(v-e.toDouble) < abs(v-r.toDouble)) e else r }

  def findNearestInt(set: Iterable[Int], v: Int): Int =
    set.foldLeft(set.head) { (r, e) => if (abs(v-e) < abs(v-r)) e else r }

  def findNearestLong(set: Iterable[Long], v: Long): Long =
    set.foldLeft(set.head) { (r, e) => if (abs(v-e) < abs(v-r)) e else r }


  def run[A](stateOps: RespState[StateMap, A]) = runOps(stateOps).get


  def bv(bit: Int): Word = {
    require(bit < nWordBits)
    1<<bit
  }

  def bm(nBits: Int): Word = bv(nBits) -1


  trait BitSelection extends Ordered [BitSelection] {
    def valueMask: Word

    def firstBit: Int
    
    def compare(that: BitSelection) = this.firstBit compare that.firstBit
    
    def bitMask: Word
    
    def asString: String

    def apply(value: Word) = (value & bitMask) >>> firstBit
    
    def unapply(value: Word): Option[Word]
  }

  
  trait BitLike extends BitSelection {
    val n:Int

    require ( (n >= 0) && (n < nWordBits) )

    def valueMask = 1
    
    def firstBit = n
    
    def bitMask = 1 << n
    
    def asString = n.toString
    
    def unapply(value: Word) : Option[Word] = {
      if ( (value == 0) || (value == 1) )
        Some ( if (value > 0) bitMask else 0 )
      else None
    }
  }

  case class Bit(n: Int) extends BitLike
  

  trait BitRangeLike extends BitSelection {
    val from: Int
    val to: Int
  
    require ( (from >= 0) && (to >= from ) && (to < nWordBits) )

    def size = (to - from + 1)
    
    def valueMask = bm(size)
    
    def firstBit = from

    def bitMask = valueMask << from
    
    def asString = if (size > 1) "%s..%s".format(from, to) else from.toString

    def unapply(value: Word) : Option[Word] = {
      if ( (value & ~valueMask) == 0 )
        Some ( (value << from) & bitMask )
      else None
    }
  }

  case class BitRange(from: Int, to: Int) extends BitRangeLike


  trait Register { register =>
    def fields = for {
      method <- register.getClass.getDeclaredMethods.toList
      if method.getParameterTypes.isEmpty
      if classOf[BitSelection].isAssignableFrom(method.getReturnType)
    } yield {
      method.getName -> method.invoke(register).asInstanceOf[BitSelection]
    }
    
    def apply(value: Word) =
      fields map { _ match { case (name, bits) => name -> bits(value) } }
  }

  
  abstract class Memory
    extends Iterable[(Address,Word)] with Function[Address, Word]
  {
    override def toString =
      iterator.toSeq map (e => "0x%s: 0x%s\n".format(hex(e._1), hex(e._2))) mkString("")
  }
  




  case class MemCache(mem: MemoryLink, contents: MemMap = IntMap.empty[Word]) extends Logging {
    def cache(addr: Address): MemCache = {
      if (contents contains addr) this
      else {
        trace("reading from 0x%s, 0x%s bytes".format(hex(addr), hex(sizeOf[Int])))
        val bytes = mem.read(addr, sizeOf[Int])
        val word = (LittleEndian.fromBytes[Int](bytes)).head
        this.copy(contents = contents + (addr -> word))
      }
    }

    def apply(addr: Address): Word = contents(addr)
  }


  case class WordMod(setBits: Word, clearBits: Word) {
    require((setBits & clearBits) == 0)
    def +(that: WordMod) =
      WordMod( this.setBits | that.setBits, this.clearBits | that.clearBits)
  }

  type MemResultOps = collection.immutable.Queue[(Address, Word => Unit)]


  case class MemChanges(mods: IntMap[WordMod] = IntMap.empty[WordMod]) {
    def +(addr: Address, bitSel: BitSelection, value: Word) : MemChanges = {
      val bitSel(toSet) = value
      val bitSel(toClear) = ~value & bitSel.valueMask
      val mod = WordMod(toSet, toClear)
      MemChanges(mods + (addr -> (mods.get(addr) map {_ + mod} getOrElse mod)))
    }
  }

  val DoOnMemoryNothing = DoOnMemory(collection.immutable.Queue.empty[(Address, Word => Unit)], IntMap.empty[Word], MemChanges())
  
  case class DoOnMemory(reads: MemResultOps, writes: MemMap, changes: MemChanges) extends Logging {
    def pause(mem: MemoryLink): DoOnMemory = {
      val r = exec(mem)
      mem.pause()
      r
    }
  
    def sync(mem: MemoryLink): DoOnMemory = {
      val r = exec(mem)
      mem.sync()
      r
    }
  
    def exec(mem: MemoryLink): DoOnMemory = {
      val mustRead = reads.map{_._1} ++ changes.mods.keys
      val cache = mustRead.foldLeft(MemCache(mem)) { (cache, addr) => cache.cache(addr) }
      
      // trace("exec(): changes = " + changes)
      
      val allWrites = changes.mods.foldLeft(writes) { (writes, modEntry) =>
        val (addr, mod) = modEntry
        val word = (cache(addr) & ~mod.clearBits | mod.setBits)
        // trace("exec(): added mod %s as write 0x%s".format(mod, hex(word)))
        writes + (addr -> word )
      }
      
      for ((addr, word) <- allWrites) {
        trace("exec(): writing to 0x%s: 0x%s".format(hex(addr), hex(word)))
        mem.write(addr, LittleEndian.toBytes(Seq(word)))
      }

      for ((addr, reaction) <- reads) reaction(cache(addr))
      
      DoOnMemoryNothing
    }
  }


  case class MemOperator(mem: MemoryLink) extends Logging {
    protected def transform[R](f: DoOnMemory => (DoOnMemory, R)) = RespStates.transform[StateMap, R] { s =>
      val ops = s.get[DoOnMemory](mem) getOrElse DoOnMemoryNothing
      val (newOps, result) = f(ops)
      (s.set(mem)(newOps), result)
    }
    
    def read(addr: Address) = transform[DelayedVal[Word]] { ops =>
      trace("read(0x%s)".format(hex(addr)))
      val result = new DelayedResult[Word](10000)
      val newOps = ops.copy(reads = ops.reads.enqueue(addr, {word => result.set(Ok(word)); trace("read(%s) set".format(addr))} ) )
      (newOps, result)
    }

    def read(addr: Address, bitSel: BitSelection) = transform[DelayedVal[Word]] { ops =>
      trace("read(0x%s, %s)".format(hex(addr), bitSel))
      val result = new DelayedResult[Word](10000)
      val newOps = ops.copy(reads = ops.reads.enqueue(addr, {word =>
        val bits = bitSel(word)
        result.set(Ok(bits))
        trace("read(0x%s, %s) set".format(hex(addr), bitSel))
      } ) )
      (newOps, result)
    }

    def write(addr: Address, value: Word) = transform[Unit] { ops =>
      // Multiple writes to the same address are not allowed
      trace("write(0x%s, 0x%s)".format(hex(addr), hex(value)))
      require(! ops.writes.contains(addr))
      (ops.copy(writes = ops.writes + (addr -> value)), ())
    }

    def jkWrite(addr: Address, value: Word) = transform[Unit] { ops =>
      // Multiple writes to the same address get XORed (necessary for J/K Register support),
      // bits must not overlap
      trace("jkWrite(0x%s, 0x%s)".format(hex(addr), hex(value)))
      val toWrite = ops.writes.get(addr) map {current =>
        require((current & value) == 0)
        current | value
      } getOrElse value
      (ops.copy(writes = ops.writes + (addr -> toWrite)), ())
    }
   
    def write(addr: Address, bitSel: BitSelection, value: Word) = transform[Unit] { ops =>
      trace("write(0x%s, %s, 0x%s)".format(hex(addr), bitSel, hex(value)))
      (ops.copy(changes = ops.changes + (addr, bitSel, value)), ())
    }
    
    def pause() = transform[Unit] { ops =>
      trace("pause()")
      (ops.pause(mem), ())
    }

    def sync() = transform[Unit] { ops =>
      trace("sync()")
      (ops.sync(mem), ())
    }

    def exec() = transform[Unit] { ops =>
      trace("exec()")
      (ops.exec(mem), ())
    }
  }

  

  def smState = RespStates.init[StateMap]

  def apply (vmeBus: VMEBus, baseAddress: Int) : SIS3300 =
    start(new SIS3300(vmeBus, baseAddress))
  
  def apply (host: String, port: Int, baseAddress: Int) : SIS3300 =
    SIS3300(VMESCPIClient(host, port), baseAddress)


    
  class MemRegion(val op: MemOperator, val base: Address) {
    region =>

    // SIS3300 registers 0x00 and 0x10 are implemented as J/K registers.
    protected def isJK(addr: Address) = ((addr) == 0x00 || (addr) == 0x10)
    protected val jkSet = BitRange(0, 15)
    protected val jkClear = BitRange(16, 31)
    
    trait ReadableRegister extends Register { register =>
      def r = new {
        def fields = for {
          method <- register.getClass.getDeclaredMethods.toList
          if method.getParameterTypes.isEmpty
          if classOf[ReadableBits].isAssignableFrom(method.getReturnType)
        } yield {
          method.getName -> method.invoke(register).asInstanceOf[ReadableBits]
        }
        
        def apply(value: Word) =
          fields map { _ match { case (name, bits) => name -> bits(value) } }
      }

      def addr: Address
      def get() = region.read(addr)
      def get(bitSel: BitSelection) = region.read(addr, bitSel)

      trait ReadableBits extends BitSelection {
          def get() = register.get(this)
      }
      case class ROBit(n: Int) extends BitLike with ReadableBits
      case class ROBitRange(from: Int, to: Int) extends BitRangeLike with ReadableBits
    }

    trait WriteableRegister extends Register { register =>
      def w = new {
        def fields = for {
          method <- register.getClass.getDeclaredMethods.toList
          if method.getParameterTypes.isEmpty
          if classOf[WriteableBits].isAssignableFrom(method.getReturnType)
        } yield {
          method.getName -> method.invoke(register).asInstanceOf[WriteableBits]
        }
        
        def apply(value: Word) =
          fields map { _ match { case (name, bits) => name -> bits(value) } }
      }

      def addr: Address
      def set(value: Word) = region.write(addr, value)
      def set(bitSel: BitSelection, value: Word) = region.write(addr, bitSel, value)

      trait WriteableBits extends BitSelection {
        def set(value: Word) = register.set(this, value)
      }
      case class WOBit(n: Int) extends BitLike with WriteableBits
      case class WOBitRange(from: Int, to: Int) extends BitRangeLike with WriteableBits
      case class COBit(n: Int) extends BitLike with BitSelection
        { def clear() = register.set(this, 0) }
      case class SOBit(n: Int) extends BitLike with BitSelection
        { def set() = register.set(this, 1) }
    }
    
    
    class RORegister(val addr: Address) extends ReadableRegister

    class WORegister(val addr: Address) extends WriteableRegister

    class RWRegister(val addr: Address) extends ReadableRegister with WriteableRegister {
      case class RWBit(n: Int) extends BitLike with ReadableBits with WriteableBits
      case class RWBitRange(from: Int, to: Int) extends BitRangeLike with ReadableBits with WriteableBits
    }
    
    class JKRegister(val addr: Address) extends ReadableRegister with WriteableRegister {
      case class RWBit(n: Int) extends BitLike with ReadableBits with WriteableBits { require(n < jkSet.size) }
      case class RWBitRange(from: Int, to: Int) extends BitRangeLike with ReadableBits with WriteableBits { require(to < jkSet.size) }
      
      override def set(value: Word) = {
        val jkSet(jBits) = value
        val jkClear(kBits) = ~value & jkClear.valueMask
        region.jkWrite(addr, jBits | kBits)
      }
      
      override def set(bitSel: BitSelection, value: Word) = {
        val bitSel(jkSet(jBits)) = value
        val bitSel(jkClear(kBits)) = ~value & bitSel.valueMask
        val toWrite = jBits | kBits
        region.jkWrite(addr, toWrite)
      }
    }
    
    class KeyRegister(val addr: Address) {
       def set() = region.write(addr, 0)
    }
    
    def read(addr: Address) = op.read(addr + base)

    def write(addr: Address, value: Word) = op.write(addr + base, value)

    def jkWrite(addr: Address, value: Word) = op.jkWrite(addr + base, value)

    def read(addr: Address, bitSel: BitSelection) = op.read(addr + base, bitSel)

    def write(addr: Address, bitSel: BitSelection, value: Word) = op.write(addr + base, bitSel, value)

    def sync() = op.sync()
  }
  
  
  
  class SISMemory(mem: MemoryLink, base: Address) extends MemRegion(MemOperator(mem), base) {

    def read(range: Range): Seq[Word] = {
      val bytes = mem.read(range.head + base, range.last - range.head + sizeOf[Word])
      // Why does the endianess depend on read size here? Property of the VME interface or the
      // SIS3300?
      if (range.size <= 8) LittleEndian.fromBytes[Word](bytes)
      else BigEndian.fromBytes[Word](bytes)
    }


    /*Control/Status Register (0x0) */
    val CONTROL_STATUS = new JKRegister(0x0) {
      /** User LED (1=LED on, 0=LED off) */
      def USRLED = RWBit(0)
      /** User Output (1=output on, 0=output off) */
      def USROUT = RWBit(1)
      /**
       * User/trigger output (1=trigger output, 0=user output). Note: In multiplexer mode,
       * output is set by multiplexer out pulse.
       */
      def USRTRGOUT = RWBit(2)
      /** Trigger output inversion (1: inverted, 0: straight) */
      def TRGOUTINV = RWBit(4)
      /** Trigger generation (1: On armed and started, 0: On armed) */
      def TRGARMST = RWBit(5)
      /** Trigger routing (1: Route to input, 0: Don't route) */
      def TRGROUTE = RWBit(6)
      /** Bank full pulse on LEMO output 1 */
      def BKFULLOUT1 = RWBit(8)
      /** Bank full pulse on LEMO output 2 */
      def BKFULLOUT2 = RWBit(9)
      /** Bank full pulse on LEMO output 3 */
      def BKFULLOUT3 = RWBit(10)
      /** Time stamp "don't clear" bit */
      def TSNOCLR = RWBit(12)

      /** Status User Input */
      def USERIN = ROBit(16)
      /** P2_TEST_IN */
      def P2_TEST_IN = ROBit(17)
      /** P2_RESET_IN */
      def P2_RESET_IN = ROBit(18)
      /** P2_SAMPLE_IN */
      def P2_SAMPLE_IN = ROBit(19)
    }

    /** Module Id. and Firmware Revision (0x4) */
    val MODID = new RORegister(0x4) {
      /* Minor Revision */
      def MINREV = ROBitRange(0, 7)
      /* Major Revision */
      def MAJREV = ROBitRange(8, 15)
      /* Module ID, should be 0x3300 or 0x3301 */
      def MODID = ROBitRange(16, 31)
    }
    
    /** Interrupt configuration register (0x8, read/write) */
    val IRQ_CONFIG = new RWRegister(0x8) {
      // Interrupt Configuration Register (IRQ_CONFIG) Bits
      /** VME IRQ Vector */
      def VECTOR = RWBitRange(0, 7)
      /** VME IRQ Level */
      def LEVEL = RWBitRange(8, 10)
      /** VME IRQ Enable (0=IRQ disabled, 1=IRQ enabled) 0 */
      def IRQEN = RWBit(11)
      /** RORA/ROAK Mode (0: RORA; 1: ROAK) 0 */
      def ROAK = RWBit(12)
    }
    
    /** Interrupt control register, write access (0xC,  J/K partial read/write) */
    val IRQ_CONTROL = new JKRegister(0xC) {
      /** Enable IRQ source 0 (end of event) */
      def SRC0EN = RWBit(0)
      /** Enable IRQ source 1 (end of last event, bank full) */
      def SRC1EN = RWBit(1)
      /** Enable IRQ source 2 (reserved) */
      def SRC2EN = RWBit(2)
      /** Enable IRQ source 3 (user input) */
      def SRC3EN = RWBit(3)
      /** Clear IRQ source 0 */
      def SRC0CLR = COBit(4)
      /** Clear IRQ source 1 */
      def SRC1CLR = COBit(5)
      /** Clear IRQ source 2 */
      def SRC2CLR = COBit(6)
      /** Clear IRQ source 3 */
      def SRC3CLR = COBit(7)

      /** Status flag source 0 (end of event) */
      def SRC0SF = ROBit(20)
      /** Status flag source 1 (end of last event, bank full) */
      def SRC1SF = ROBit(21)
      /** Status flag source 2 (reserved) */
      def SRC2SF = ROBit(22)
      /** Status flag source 3 (user input) */
      def SRC3SF = ROBit(23)
      /** Status internal IRQ 0 */
      def IIRQST = ROBit(26)
      /** Status VME IRQ 0 */
      def VIRQST = ROBit(27)
      /** Status IRQ source 0 */
      def SRC0IS = ROBit(28)
      /** Status IRQ source 1 */
      def SRC1IS = ROBit(29)
      /** Status IRQ source 2 */
      def SRC2IS = ROBit(30)
      /** Status IRQ source 3ICTL_SRC3_IS  31  // Status IRQ source 3 */
      def SRC3IS = ROBit(31)
    }

    /** Acquisition control register (0x10, J/K-read/write) */
    val ACQUISITION_CONTROL = new JKRegister(0x10) {
      /** Enable sample clock for bank 1 */
      def SCLOCKB1_EN = RWBit(0)
      /** Enable sample clock for bank 2 */
      def SCLOCKB2_EN = RWBit(1)
      /** Enable auto bank switch mode */
      def AUTOBANK_EN = RWBit(2)
      /** Use delay locked loop for external clock(SIS3301 03 06 only) */
      def DLLEXT_EN = RWBit(3)
      /** Enable Autostart (in multi event mode only ) */
      def AUTOSTART_EN = RWBit(4)
      /** Enable multi event mode */
      def MULTIEVENT_EN = RWBit(5)
      /** Enable start delay */
      def STARTDELAY_EN = RWBit(6)
      /** Enable stop delay */
      def STOPDELAY_EN = RWBit(7)
      /** Enable front panel Lemo Start/Stop logic */
      def FPLEMOSS_EN = RWBit(8)
      /** Enable P2 Start/Stop logic */
      def P2SS_EN = RWBit(9)
      /** Enable front panel gate mode (not Start/Stop) */
      def FPGATE_EN = RWBit(10)
      /** Enable external clock random mode */
      def RNDCLOCK_EN = RWBit(11)
      /** Set clock source */
      def CLOCKSRC = RWBitRange(12, 14)
      /** Set multiplexer mode */
      def MPLEXMODE_SET = RWBit(15)

      /** ADC busy */
      def ADC_BUSY = ROBit(16)
      /** Bank switch busy */
      def BANKSW_BUSY = ROBit(18)
      /** Bank 1 busy */
      def BANK1_BUSY = ROBit(20)
      /** Bank 1 full */
      def BANK1_FULL = ROBit(21)
      /** Bank 2 busy */
      def BANK2_BUSY = ROBit(22)
      /** Bank 2 full */
      def BANK2_FULL = ROBit(23)
    }

    /** Start Delay register (0x14, read/write) */
    val START_DELAY = new RWRegister(0x14) {
      def STARTDEL = RWBitRange(0, 15)
    }

    /** Stop Delay register (0x18, read/write) */
    val STOP_DELAY = new RWRegister(0x18) {
      def STOPDEL = RWBitRange(0, 15)
    }
    
    /** Time stamp predivider register (0x1C, read/write) */
    val TIMESTAMP_PREDIVIDER = new RWRegister(0x1C) {
      def TSPREDEV = RWBitRange(0, 15)
    }
    
    /** Key address general reset (0x20, write-only) */
    val KEY_RESET = new KeyRegister(0x20)

    /** Key address VME start sampling (0x30, write-only) */
    val KEY_START = new KeyRegister(0x30)

    /** Key address VME stop sampling (0x34, write-only) */
    val KEY_STOP = new KeyRegister(0x34)

    /** Key address start Auto Bank Switch mode (0x40, write-only) */
    val KEY_START_AUTO_BANK_SWITCH = new KeyRegister(0x40)
    
    /** Key address stop Auto Bank Switch mode (0x44, write-only) */
    val KEY_STOP_AUTO_BANK_SWITCH = new KeyRegister(0x44)
    
    /** Key address clear BANK1 FULL Flag (0x48, write-only) */
    val KEY_BANK1_FULL_FLAG = new KeyRegister(0x48)
    
    /** Key address clear BANK2 FULL Flag (0x4C, write-only) */
    val KEY_BANK2_FULL_FLAG = new KeyRegister(0x4C)

    
    // /** Event configuration register, all ADCs (0x100000, write-only) */
    // val EVENT_CONFIG_ALL_ADC = new WORegister(0x100000) {
    //   /** Page size */
    //   def PGSIZE = WOBitRange(0, 2)
    //   /** Wrap around mode (1: Wrap until STOP (External or KEY), 0: Autostop at end of page) */
    //   def WRAP = WOBit(3)
    //   /** Enable gate chaining mode */
    //   def GATECHAIN = WOBit(4)
    //   /** Channel Group ID Bit */
    //   def GROUPID = WOBitRange(8, 9)
    //   /** External random clock mode */
    //   def EXTRNDCLK = WOBit(11)
    //   /** Multiplexer mode */
    //   def MPLEXMODE = WOBit(15)
    //   /** Average Bit 0 */
    //   def AVERAGE = WOBitRange(16, 18)
    //   /** Page size map select bit (firmware 03 08 onwards) */
    //   def PGSIZEMAPSEL = WOBit(20)
    // }
    
    /** Event configuration register, ADC group specific (read/write) */
    class EventConfigRegister(addr: Address) extends RWRegister(addr) {
      /** Page size */
      def PGSIZE = RWBitRange(0, 2)
      /** Wrap around mode (1: Wrap until STOP (External or KEY), 0: Autostop at end of page) */
      def WRAP = RWBit(3)
      /** Enable gate chaining mode */
      def GATECHAIN = RWBit(4)
      /** Channel Group ID Bit */
      def GROUPID = RWBitRange(8, 9)
      /** External random clock mode */
      def EXTRNDCLK = RWBit(11)
      /** Multiplexer mode */
      def MPLEXMODE = RWBit(15)
      /** Average Bit 0 */
      def AVERAGE = RWBitRange(16, 18)
      /** Page size map select bit (firmware 03 08 onwards) */
      def PGSIZEMAPSEL = RWBit(20)
    }
    
    /** Event configuration register, ADC group 1/2 (0x200000, read/write) */
    val EVENT_CONFIG_ADC12 = new EventConfigRegister(0x200000)
    /** Event configuration register, ADC group 1/2 (0x280000, read/write) */
    val EVENT_CONFIG_ADC34 = new EventConfigRegister(0x280000)
    /** Event configuration register, ADC group 1/2 (0x300000, read/write) */
    val EVENT_CONFIG_ADC56 = new EventConfigRegister(0x300000)
    /** Event configuration register, ADC group 1/2 (0x380000, read/write) */
    val EVENT_CONFIG_ADC78 = new EventConfigRegister(0x380000)
    

    // /** Trigger Threshold Register, all ADCs (0x100004, write-only) */
    // val THRESHOLD_ALL_ADC = new WORegister(0x100004) {
    //   /** Threshold value, even-numbered ADCs (2/4/6/8) */
    //   def THRESHEVEN = WOBitRange(0, 13)
    //   /** GT or LE, even-numbered ADCs (0: GT, 1: LE) */
    //   def GTLEEVEN = WOBit(15)
    //   /** Threshold value, odd-numbered ADCs (1/3/5/7) */
    //   def THRESHODD = WOBitRange(16, 29)
    //   /** GT or LE, odd-numbered ADCs (0: GT, 1: LE) */
    //   def GTLEODD = WOBit(31)
    // }

    /** Trigger Threshold Register, ADC group specific (read/write) */
    class TriggerThresholdRegister(addr: Address) extends RWRegister(addr) {
      /** Threshold value, even-numbered ADCs (2/4/6/8) */
      def THRESHEVEN = RWBitRange(0, 11)
      /** GT or LE, even-numbered ADCs (0: GT, 1: LE) */
      def GTLEEVEN = RWBit(15)
      /** Threshold value, odd-numbered ADCs (1/3/5/7) */
      def THRESHODD = RWBitRange(16, 27)
      /** GT or LE, odd-numbered ADCs (0: GT, 1: LE) */
      def GTLEODD = RWBit(31)
    }

    /** Trigger Threshold Register, ADC group 1/2 (0x200004, read/write) */
    val TRIGGER_THRESHOLD_ADC12 = new TriggerThresholdRegister(0x200004)
    /** Trigger Threshold Register, ADC group 3/4 (0x280004, read/write) */
    val TRIGGER_THRESHOLD_ADC34 = new TriggerThresholdRegister(0x280004)
    /** Trigger Threshold Register, ADC group 5/6 (0x300004, read/write) */
    val TRIGGER_THRESHOLD_ADC56 = new TriggerThresholdRegister(0x300004)
    /** Trigger Threshold Register, ADC group 7/8 (0x380004, read/write) */
    val TRIGGER_THRESHOLD_ADC78 = new TriggerThresholdRegister(0x380004)


    /** Trigger Flag Clear Counter register, all ADCs (0x10001C, write-only) */
    val TRIGGER_FLAG_CLR_CNT_ALL_ADC = new WORegister(0x10001C) {
      /** Trigger Flag Clear counter register */
      def CLRCNT = WOBitRange(0, 15)
    }

    /** Trigger Flag Clear Counter register, ADC group specific (read/write) */
    class TriggerFlagClearCounterRegister(addr: Address) extends RWRegister(addr) {
      /** Trigger Flag Clear counter register */
      def CLRCNT = RWBitRange(0, 15)
    }

    /** Trigger Flag Clear Counter register, ADC group 1/2 (0x20001C, read/write) */
    val TRIGGER_FLAG_CLR_CNT_ADC12 = new TriggerFlagClearCounterRegister(0x20001C)
    /** Trigger Flag Clear Counter register, ADC group 3/4 (0x28001C, read/write) */
    val TRIGGER_FLAG_CLR_CNT_ADC34 = new TriggerFlagClearCounterRegister(0x28001C)
    /** Trigger Flag Clear Counter register, ADC group 5/6 (0x30001C, read/write) */
    val TRIGGER_FLAG_CLR_CNT_ADC56 = new TriggerFlagClearCounterRegister(0x30001C)
    /** Trigger Flag Clear Counter register, ADC group 7/8 (0x38001C, read/write) */
    val TRIGGER_FLAG_CLR_CNT_ADC78 = new TriggerFlagClearCounterRegister(0x38001C)


    /** Clock Predivider register, all ADCs (0x100020, write-only) */
      val CLOCK_PREDIVIDER_ALL_ADC = new WORegister(0x100020) {
      /** Clock Predivider */
      def CLKPREDEV = WOBitRange(0, 7)
    }
    
    /** Clock Predivider register, ADC group specific (read/write) */
    class ClockPredeviderRegister(addr: Address) extends RWRegister(addr) {
      /** Clock Predivider */
      def CLKPREDEV = RWBitRange(0, 7)
    }

    /** Clock Predivider register, ADC group 1/2 (0x20001C, read/write) */
    val CLOCK_PREDIVIDER_ADC12 = new ClockPredeviderRegister(0x200020)
    /** Clock Predivider register, ADC group 3/4 (0x28001C, read/write) */
    val CLOCK_PREDIVIDER_ADC34 = new ClockPredeviderRegister(0x280020)
    /** Clock Predivider register, ADC group 5/6 (0x30001C, read/write) */
    val CLOCK_PREDIVIDER_ADC56 = new ClockPredeviderRegister(0x300020)
    /** Clock Predivider register, ADC group 7/8 (0x38001C, read/write) */
    val CLOCK_PREDIVIDER_ADC78 = new ClockPredeviderRegister(0x380020)

    
    /** No_Of_Sample register, all ADCs (0x100024, write-only) */
    val NO_OF_SAMPLE_ALL_ADC = new WORegister(0x100024) {
      /** No_Of_Sample */
      def NOFSAMPLE = WOBitRange(0, 7)
    }

    /** No_Of_Sample register, ADC group specific (read/write) */
    class NoOfSampleRegister(addr: Address) extends RWRegister(addr) {
      /** No_Of_Sample */
      def NOFSAMPLE = RWBitRange(0, 7)
    }

    /** No_Of_Sample register, ADC group 1/2 (0x200024, read/write) */
    val NO_OF_SAMPLE_ADC12 = new NoOfSampleRegister(0x200024)
    /** No_Of_Sample register, ADC group 3/4 (0x280024, read/write) */
    val NO_OF_SAMPLE_ADC34 = new NoOfSampleRegister(0x280024)
    /** No_Of_Sample register, ADC group 5/6 (0x300024, read/write) */
    val NO_OF_SAMPLE_ADC56 = new NoOfSampleRegister(0x300024)
    /** No_Of_Sample register, ADC group 7/8 (0x380024, read/write) */
    val NO_OF_SAMPLE_ADC78 = new NoOfSampleRegister(0x380024)


    // /** Trigger setup register, all ADCs (0x100028, write-only) */
    // val TRIGGER_SETUP_ALL_ADC = new WORegister(0x100028) {
    //  /** M */
    //   def M = WOBitRange(0, 3)
    //   /** P */
    //   def N = WOBitRange(8, 11)
    //   /** P */
    //   def P = WOBitRange(16, 19)
    //   /** enable N M mode */
    //   def NM = WOBit(24)
    //   /** enable pulse mode */
    //   def PULSE = WOBit(28)
    // }

    /** Trigger setup register, ADC group specific (read/write) */
    class TriggerSetupRegister(addr: Address) extends RWRegister(addr) {
     /** M */
      def M = RWBitRange(0, 3)
      /** P */
      def N = RWBitRange(8, 11)
      /** P */
      def P = RWBitRange(16, 19)
      /** enable N M mode */
      def NM = RWBit(24)
      /** enable pulse mode */
      def PULSE = RWBit(28)
    }

    /** Trigger setup register, ADC group 1/2 (0x200028, read/write) */
    val TRIGGER_SETUP_ADC12 = new TriggerSetupRegister(0x200028)
    /** Trigger setup register, ADC group 3/4 (0x280028, read/write) */
    val TRIGGER_SETUP_ADC34 = new TriggerSetupRegister(0x280028)
    /** Trigger setup register, ADC group 5/6 (0x300028, read/write) */
    val TRIGGER_SETUP_ADC56 = new TriggerSetupRegister(0x300028)
    /** Trigger setup register, ADC group 7/8 (0x380028, read/write) */
    val TRIGGER_SETUP_ADC78 = new TriggerSetupRegister(0x380028)


    /** MAX No of Events register, all ADCs (0x10002C, write-only) */
    val MAX_NO_OF_EVENTS_ALL_ADC = new WORegister(0x10002C) {
      /** Max_No_Of_Events */
      def MAXNEV = WOBitRange(0, 15)
    }

    /** MAX No of Events register, ADC group specific (read/write) */
    class MaxNoOfEventsRegister(addr: Address) extends RWRegister(addr) {
      /** Max_No_Of_Events */
      def MAXNEV = RWBitRange(0, 15)
    }

    /** MAX No of Events register, ADC group 1/2 (0x20002C, read/write) */
    val MAX_NO_OF_EVENTS_ADC12 = new MaxNoOfEventsRegister(0x20002C)
    /** MAX No of Events register, ADC group 3/4 (0x28002C, read/write) */
    val MAX_NO_OF_EVENTS_ADC34 = new MaxNoOfEventsRegister(0x28002C)
    /** MAX No of Events register, ADC group 5/6 (0x30002C, read/write) */
    val MAX_NO_OF_EVENTS_ADC56 = new MaxNoOfEventsRegister(0x30002C)
    /** MAX No of Events register, ADC group 7/8 (0x38002C, read/write) */
    val MAX_NO_OF_EVENTS_ADC78 = new MaxNoOfEventsRegister(0x38002C)

    /** Trigger event directory entry */
    val TriggerEventDirEntry = new Register {
      /** End Address + 1 of Event*/
      def EVEND = BitRange(0, 16)
      /** Event Data Wrapped */
      def WRAPPED = Bit(19)
      /** Trigger n Triggered (lsb: T8 .. msb: T1) */
      def TRIGGED = BitRange(24,31)
    }
    
    /** Trigger event directory bank 1 (0x101000 - 0x101ffc, read-only) */
    val EVENT_DIRECTORY_BANK1 = RegisterRange(0x101000, 0x102000)
    /** Trigger event directory bank 2 (0x102000 - 0x102ffc, read-only) */
    val EVENT_DIRECTORY_BANK2 = RegisterRange(0x102000, 0x103000)


    /** Event counter register, ADC group specific (read/write) */
    class EventCounterRegister(addr: Address) extends RORegister(addr) {
      /** Event counter */
      def NEVENTS = ROBitRange(0, 15)
    }

    /** Event counter register, Bank 1, ADC group 1/2 (0x200010, read-only) */
    val BANK1_EVENT_CNT_ADC12 = new EventCounterRegister(0x200010)
    /** Event counter register, Bank 1, ADC group 3/4 (0x280010, read-only) */
    val BANK1_EVENT_CNT_ADC34 = new EventCounterRegister(0x280010)
    /** Event counter register, Bank 1, ADC group 5/6 (0x300010, read-only) */
    val BANK1_EVENT_CNT_ADC56 = new EventCounterRegister(0x300010)
    /** Event counter register, Bank 1, ADC group 7/8 (0x380010, read-only) */
    val BANK1_EVENT_CNT_ADC78 = new EventCounterRegister(0x380010)

    /** Event counter register, Bank 2, ADC group 1/2 (0x200010, read-only) */
    val BANK2_EVENT_CNT_ADC12 = new EventCounterRegister(0x200010)
    /** Event counter register, Bank 2, ADC group 3/4 (0x280010, read-only) */
    val BANK2_EVENT_CNT_ADC34 = new EventCounterRegister(0x280010)
    /** Event counter register, Bank 2, ADC group 5/6 (0x300010, read-only) */
    val BANK2_EVENT_CNT_ADC56 = new EventCounterRegister(0x300010)
    /** Event counter register, Bank 2, ADC group 7/8 (0x380010, read-only) */
    val BANK2_EVENT_CNT_ADC78 = new EventCounterRegister(0x380010)


    /** Actual Sample register, ADC group specific (read/write) */
    class ActualSampleRegister(addr: Address) extends RORegister(addr) {
      /** Sample value, even-numbered ADCs (2/4/6/8) */
      def SAMEVEN = ROBitRange(0, 11)
      /** OR bit, even-numbered ADCs (0: GT, 1: LE) */
      def OREVEN = ROBit(12)
      /** Sample value, odd-numbered ADCs (1/3/5/7) */
      def SAMODD = ROBitRange(16, 27)
      /** OR bit, odd-numbered ADCs (0: GT, 1: LE) */
      def ORODD = ROBit(28)
    }

    /** Actual Sample register, ADC group 1/2 (0x200018, read/write) */
    val ACTUAL_SAMPLE_VALUE_ADC12 = new ActualSampleRegister(0x200018)
    /** Actual Sample register, ADC group 3/4 (0x280018, read/write) */
    val ACTUAL_SAMPLE_VALUE_ADC34 = new ActualSampleRegister(0x280018)
    /** Actual Sample register, ADC group 5/6 (0x300018, read/write) */
    val ACTUAL_SAMPLE_VALUE_ADC56 = new ActualSampleRegister(0x300018)
    /** Actual Sample register, ADC group 7/8 (0x380018, read/write) */
    val ACTUAL_SAMPLE_VALUE_ADC78 = new ActualSampleRegister(0x380018)


    /** Sample Memory Bank Entry (read/write)*/
    val BankMemoryEntry = new Register {
      /** Sample value, even-numbered ADCs (2/4/6/8) */
      def SAMEVEN = BitRange(0, 11)
      /** OR bit, even-numbered ADCs (0: GT, 1: LE) */
      def OREVEN = Bit(12)
      /** Set to 1 on the first sample in "Gate Chaining Mode", 0 otherwise  */
      def GCH = Bit(15)
      /** Sample value, odd-numbered ADCs (1/3/5/7) */
      def SAMODD = BitRange(16, 27)
      /** OR bit, odd-numbered ADCs (0: GT, 1: LE) */
      def ORODD = Bit(28)
      /** User input logic value */
      def USRIN = Bit(31)
    }
    
    /** Sample Memory, Bank 1 (0x400000 - 0x5ffffc, read/write) */
    val MEM_BANK1 = RegisterRange(0x400000, 0x600000)

    /** Sample Memory, Bank 2 (0x600000 - 0x7ffffc, read/write) */
    val MEM_BANK2 = RegisterRange(0x600000, 0x800000)

    /** Sample Memory, Bank 1, ADC group 1/2 (0x400000, read-only) */
    val MEM_BANK1_ADC12 = RegisterRange(0x400000, 0x480000)
    /** Sample Memory, Bank 1, ADC group 3/4 (0x480000, read-only) */
    val MEM_BANK1_ADC34 = RegisterRange(0x480000, 0x500000)
    /** Sample Memory, Bank 1, ADC group 5/6 (0x500000, read-only) */
    val MEM_BANK1_ADC56 = RegisterRange(0x500000, 0x580000)
    /** Sample Memory, Bank 1, ADC group 7/8 (0x580000, read-only) */
    val MEM_BANK1_ADC78 = RegisterRange(0x580000, 0x600000)

    /** Sample Memory, Bank 2, ADC group 1/2 (0x600000, read-only) */
    val MEM_BANK2_ADC12 = RegisterRange(0x600000, 0x680000)
    /** Sample Memory, Bank 2, ADC group 3/4 (0x680000, read-only) */
    val MEM_BANK2_ADC34 = RegisterRange(0x680000, 0x700000)
    /** Sample Memory, Bank 2, ADC group 5/6 (0x700000, read-only) */
    val MEM_BANK2_ADC56 = RegisterRange(0x700000, 0x780000)
    /** Sample Memory, Bank 2, ADC group 7/8 (0x780000, read-only) */
    val MEM_BANK2_ADC78 = RegisterRange(0x780000, 0x800000)


    val TrigOff = TriggerThreshold(threshold = 0xfff, polarity = true)

    val sampleRange = (0 to 0x1000)

    val clockSourceTable = Map (
      100000000L -> 0x0, 50000000L -> 0x1, 25000000L -> 0x2,
      12500000L -> 0x3, 6250000L -> 0x4, 3125000L -> 0x5,
      0L -> 0x6 // External clock
    )

    case class PGConfig(psm: Int, pgs: Int, nEvents: Int)
    
    val pageConfigTable = Map(
      (1<<17) -> PGConfig(0x0, 0x0,    1),
      // (1<<15) -> PGConfig(0x1, 0x1,    4), // not available in all fw versions
      (1<<14) -> PGConfig(0x0, 0x1,    8),
      // (1<<13) -> PGConfig(0x1, 0x2,   16), // not available in all fw versions
      (1<<12) -> PGConfig(0x0, 0x2,   32),
      (1<<11) -> PGConfig(0x0, 0x3,   64),
      (1<<10) -> PGConfig(0x0, 0x4,  128),
      (1<< 9) -> PGConfig(0x0, 0x5,  256),
      (1<< 8) -> PGConfig(0x0, 0x6,  512),
      (1<< 7) -> PGConfig(0x0, 0x7, 1024)
    )
    
    val avgConfigTable = Map((0 to 7) map (1 << _) zipWithIndex : _*)
  }

}
