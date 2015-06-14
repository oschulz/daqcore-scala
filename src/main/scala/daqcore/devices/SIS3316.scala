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
import scala.collection.breakOut
import scala.collection.immutable.Queue
import akka.actor._

import daqcore.actors._, daqcore.actors.TypedActorTraits._
import daqcore.util._
import daqcore.util.concurrent._
import daqcore.io._
import daqcore.io.memory._


trait SIS3316 extends Device {
  import SIS3316.dataTypes._

  def memory: Future[SIS3316Memory]

  def serNo: Future[String]
  def internalTemperature: Future[Double]

  def trigger_extern_enabled_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def trigger_extern_enabled_set(chV: ChV[Boolean]): Future[Unit]

  def trigger_intern_enabled_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def trigger_intern_enabled_set(chV: ChV[Boolean]): Future[Unit]

  def trigger_threshold_get(ch: Ch = allChannels): Future[ChV[Int]]
  def trigger_threshold_set(chV: ChV[Int]): Future[Unit]

  def trigger_cfd_get(ch: Ch = allChannels): Future[ChV[CfdCtrl.Value]]
  def trigger_cfd_set(chV: ChV[CfdCtrl.Value]): Future[Unit]

  def trigger_gapTime_get(ch: Ch = allChannels): Future[ChV[Int]]
  def trigger_gapTime_set(chV: ChV[Int]): Future[Unit]

  def trigger_peakTime_get(ch: Ch = allChannels): Future[ChV[Int]]
  def trigger_peakTime_set(chV: ChV[Int]): Future[Unit]

  def energy_gapTime_get(ch: Ch = allChannels): Future[ChV[Int]]
  def energy_gapTime_set(chV: ChV[Int]): Future[Unit]

  def energy_peakTime_get(ch: Ch = allChannels): Future[ChV[Int]]
  def energy_peakTime_set(chV: ChV[Int]): Future[Unit]

  def energy_tau_table_get(ch: Ch = allChannels): Future[ChV[Int]]
  def energy_tau_table_set(chV: ChV[Int]): Future[Unit]

  def energy_tau_factor_get(ch: Ch = allChannels): Future[ChV[Int]]
  def energy_tau_factor_set(chV: ChV[Int]): Future[Unit]

  def input_invert_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def input_invert_set(chV: ChV[Boolean]): Future[Unit]

  def nsamples_total_get(ch: Ch = allChannels): Future[ChV[Int]]
  def nsamples_total_set(chV: ChV[Int]): Future[Unit]

  def nsamples_pretrig_get(ch: Ch = allChannels): Future[ChV[Int]]
  def nsamples_pretrig_set(chV: ChV[Int]): Future[Unit]

  def nmaw_total_get(ch: Ch = allChannels): Future[ChV[Int]]
  def nmaw_total_set(chV: ChV[Int]): Future[Unit]

  def nmaw_pretrig_get(ch: Ch = allChannels): Future[ChV[Int]]
  def nmaw_pretrig_set(chV: ChV[Int]): Future[Unit]

  def bank_fill_threshold_nbytes_get(ch: Ch = allChannels): Future[ChV[Int]]
  def bank_fill_threshold_nbytes_set(chV: ChV[Int]): Future[Unit]

  def bank_fill_threshold_stop_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def bank_fill_threshold_stop_set(chV: ChV[Boolean]): Future[Unit]

  def forceTrig(): Future[Unit]

  def startCapture(): Future[Unit]
  def stopCapture(): Future[Unit]

  def capture_enabled_set(value: Boolean): Future[Unit]
  def capture_enabled_get: Future[Boolean]

  def getMem(register: MemRegion#ReadableRegister[Int]): Future[Int]
  def getMemConv[@specialized(Int, Long, Float, Double)  U](bits: MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[U]
  def getMemConv[@specialized(Int, Long, Float, Double)  U](bits: (Int, Int) => MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(ch: Ch)(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[ChV[U]]
  def getMemConvFPGA[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(ch: Ch)(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[ChV[U]]

  def setMem(register: MemRegion#WriteableRegister[Int], value: Int)(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[Unit]
  def setMemConv[@specialized(Int, Long, Float, Double)  U](bits: MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U], value: U)(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[Unit]
  def setMemConv[@specialized(Int, Long, Float, Double)  U](bits: (Int, Int) => MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U])(chVals: ChV[U])(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[Unit]
  def setMemConvFPGA[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U])(chVals: ChV[U])(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[Unit]

  def startFIFOReadTransfer(ch: Int, bank: Int, from: Int = 0): Future[Unit]
  def resetFIFO(ch: Int): Future[Unit]
  def readFIFOData(ch: Int, nWords: Int): Future[ByteString]

  def event_format_get(ch: Ch = Ch(1 to 16)): Future[ChV[EventFormat]]

  def sampling_status: Future[SamplingStatus]

  def newEventsAvail: Future[Boolean]
  def armBank(bank: Int): Future[Unit]
  def swapBanks(): Future[Unit]
  def clearAndArm(): Future[Unit]
  def disarm(): Future[Unit]
  def dataToRead(ch: Ch = allChannels): Future[ChV[DataToRead]]
  def readAllRawEventData(channel: Int): Future[ByteString]
  def readRawEventData(bank: Int, ch: Int, from: Int, nBytes: Int): Future[ByteString]
}


object SIS3316 extends DeviceCompanion[SIS3316] {
  object dataTypes {

    val sizeOfInt = implicitly[IntegerNumType[Int]].nBytes

    val allChannels = Ch(1 to 16)

    val CfdCtrl = SIS3316Memory.registers.CfdCtrl

    case class EvtFlags(overflow: Boolean, underflow: Boolean,  repileup: Boolean, pileup: Boolean)
    case class PSAValue(index: Int, value: Int)
    case class MAWValues(maximum: Int, preTrig: Int, postTrig: Int)
    case class EnergyValues(initial: Int, maximum: Int)


    case class RawChEvent(
      chId: Int = 0,
      timestamp: Long = 0,
      flags: Option[EvtFlags] = None,
      accSums: ChV[Int] = ChV[Int](),
      peakHeight: Option[PSAValue] = None,
      mawValues: Option[MAWValues] = None,
      energyValues: Option[EnergyValues] = None,
      pileupFlag: Boolean = false,
      samples: ArrayVec[Int] = ArrayVec[Int](),
      mawSamples: ArrayVec[Int] = ArrayVec[Int]()
    )


    case class RawEvent(
      chId: ChV[Int] = ChV[Int](),
      timestamp: ChV[Long] = ChV[Long](),
      flags: ChV[EvtFlags] = ChV[EvtFlags](),
      accSums: ChV[Int] = ChV[Int](),
      peakHeight: ChV[PSAValue] = ChV[PSAValue](),
      mawValues: ChV[MAWValues] = ChV[MAWValues](),
      energyValues: ChV[EnergyValues] = ChV[EnergyValues](),
      pileupFlag: ChV[Boolean] = ChV[Boolean](),
      samples: ChV[ArrayVec[Int]] = ChV[ArrayVec[Int]](),
      mawSamples: ChV[ArrayVec[Int]] = ChV[ArrayVec[Int]]()
    )


    case class EventFormat(
      save_maw_test: Option[Boolean] = None,  // Save (Energy or Trigger) MAW Test Data
      save_energy: Boolean = false,  // Save energy values (at trigger and maximum during trigger window)
      save_ft_maw: Boolean = false,  // Save fast trigger MAW values (max value, value before Trigger, value with Trigger)
      save_acc_78: Boolean = false,  // Save accumulator values for gates 7 and 8
      save_ph_acc16: Boolean = false,  // Save peak height and accumulator values for gates 1,2, ..,6
      nSamples: Int = 0,  // Number of raw samples to save
      nMAWTestWords: Int = 0  // Number of MAW test data words to save
    ) {
      // TODO: Add support for averaging value data format

      require(nSamples % 2 == 0)

      def rawEventDataSize = {
        val nEvtWords =
          2 +
          (if (save_ph_acc16) 7 else 0) +
          (if (save_acc_78) 2 else 0) +
          (if (save_ft_maw) 3 else 0) +
          (if (save_energy) 2 else 0) +
          1 +
          nSamples / 2 +
          (if (save_maw_test != None) nMAWTestWords else 0)

        nEvtWords * sizeOfInt
      }
    }


    case class DataToRead(
      bank: Int = 0,
      from: Int = 0,
      until: Int = 0,
      format: EventFormat = EventFormat()
    )


    case class SamplingStatus(busy: Boolean = false, armed: Boolean = false, armedBank: Int = 0)

  }


  def impl = { case uri => new SIS3316Impl(uri.toString) }

  class SIS3316Impl(vmeURI: String) extends SIS3316
    with CloseableTAImpl with SyncableImpl with LocalECTypedActorImpl
  {
    import SIS3316Impl._
    import SIS3316Memory.eventFormat._
    import dataTypes._

    val registers = SIS3316Memory.registers

    val mem = SIS3316Memory(vmeURI, "memory")

    def memory = successful(mem)

    override def sync() = mem.sync()
    override def getSync() = mem.getSync()

    def identity = localExec( async {
      val modId = getMemConv(registers.modid.module_id)
      val fwType = getMemConv(registers.modid.module_id)
      s"SIS${await(modId)}-${await(fwType)}"
    } (_) )


    def serNo = mem.read(registers.serial_number_reg) map { x => x.toString }

    def internalTemperature = getMemConv(registers.internal_temperature_reg.temperature)

    def trigger_extern_enabled_get(ch: Ch) = getMemConv(registers.fpga(_).event_config_reg.ch(_).ext_trig_en)(ch)
    def trigger_extern_enabled_set(chV: ChV[Boolean]) = setMemConv(registers.fpga(_).event_config_reg.ch(_).ext_trig_en)(chV)


    def trigger_intern_enabled_get(ch: Ch) = getMemConv(registers.fpga(_).event_config_reg.ch(_).int_trig_en)(ch)

    def trigger_intern_enabled_set(chV: ChV[Boolean]) = {
      setMemConv(registers.fpga(_).event_config_reg.ch(_).int_trig_en)(chV)
      setMemConv(registers.fpga(_).fir_trigger_threshold_reg(_).trig_en)(chV)
    }


    def trigger_threshold_get(ch: Ch) = getMemConv(registers.fpga(_).fir_trigger_threshold_reg(_).threshold)(ch)
    def trigger_threshold_set(chV: ChV[Int]) = setMemConv(registers.fpga(_).fir_trigger_threshold_reg(_).threshold)(chV)

    def trigger_cfd_get(ch: Ch) = getMemConv(registers.fpga(_).fir_trigger_threshold_reg(_).cfd_ctrl)(ch) map { _.vMap {_.asInstanceOf[CfdCtrl.Value]} }
    def trigger_cfd_set(chV: ChV[CfdCtrl.Value]) = setMemConv(registers.fpga(_).fir_trigger_threshold_reg(_).cfd_ctrl)(chV)

    def trigger_gapTime_get(ch: Ch) = getMemConv(registers.fpga(_).fir_trigger_setup_reg(_).gap_time)(ch)
    def trigger_gapTime_set(chV: ChV[Int]) = setMemConv(registers.fpga(_).fir_trigger_setup_reg(_).gap_time)(chV)

    def trigger_peakTime_get(ch: Ch) = getMemConv(registers.fpga(_).fir_trigger_setup_reg(_).peak_time)(ch)
    def trigger_peakTime_set(chV: ChV[Int]) = setMemConv(registers.fpga(_).fir_trigger_setup_reg(_).peak_time)(chV)

    def energy_gapTime_get(ch: Ch) = getMemConv(registers.fpga(_).fir_energy_setup_reg(_).gap_time)(ch)
    def energy_gapTime_set(chV: ChV[Int]) = setMemConv(registers.fpga(_).fir_energy_setup_reg(_).gap_time)(chV)

    def energy_peakTime_get(ch: Ch) = getMemConv(registers.fpga(_).fir_energy_setup_reg(_).peak_time)(ch)
    def energy_peakTime_set(chV: ChV[Int]) = setMemConv(registers.fpga(_).fir_energy_setup_reg(_).peak_time)(chV)

    def energy_tau_table_get(ch: Ch) = getMemConv(registers.fpga(_).fir_energy_setup_reg(_).tau_table)(ch)
    def energy_tau_table_set(chV: ChV[Int]) = setMemConv(registers.fpga(_).fir_energy_setup_reg(_).tau_table)(chV)

    def energy_tau_factor_get(ch: Ch) = getMemConv(registers.fpga(_).fir_energy_setup_reg(_).tau_factor)(ch)
    def energy_tau_factor_set(chV: ChV[Int]) = setMemConv(registers.fpga(_).fir_energy_setup_reg(_).tau_factor)(chV)

    def input_invert_get(ch: Ch) = getMemConv(registers.fpga(_).event_config_reg.ch(_).input_inv)(ch)
    def input_invert_set(chV: ChV[Boolean]) = setMemConv(registers.fpga(_).event_config_reg.ch(_).input_inv)(chV)

    def nsamples_total_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).raw_data_buffer_config_reg.sample_length)(ch)
    def nsamples_total_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).raw_data_buffer_config_reg.sample_length)(chV)

    def nsamples_pretrig_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).pre_trigger_delay_reg.delay)(ch)
    def nsamples_pretrig_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).pre_trigger_delay_reg.delay)(chV)

    def nmaw_total_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).maw_test_buffer_config_reg.buffer_len)(ch)
    def nmaw_total_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).maw_test_buffer_config_reg.buffer_len)(chV)

    def nmaw_pretrig_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).maw_test_buffer_config_reg.pretrig_delay)(ch)
    def nmaw_pretrig_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).maw_test_buffer_config_reg.pretrig_delay)(chV)

    def bank_fill_threshold_nbytes_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).address_threshold_reg.addr_thresh_value)(ch)
    def bank_fill_threshold_nbytes_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).address_threshold_reg.addr_thresh_value)(chV)

    def bank_fill_threshold_stop_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).address_threshold_reg.stop_on_thresh)(ch)
    def bank_fill_threshold_stop_set(chV: ChV[Boolean]) = setMemConvFPGA(registers.fpga(_).address_threshold_reg.stop_on_thresh)(chV)


    def forceTrig() = {
      val done = mem.write(registers.key_trigger)
      mem.sync()
      done
    }


    def startCapture() = capture_enabled_set(true)

    def stopCapture() = capture_enabled_set(false)

    def capture_enabled_set(value: Boolean) = {
      if (capture_enabled != value) {
        log.trace(s"Setting capture_enabled to $value")
        value match {
          case true =>
            capture_enabled = true
            clearAndArm()
            pollNewEvents()
          case false =>
            disarm()
            capture_enabled = false
            scheduledPoll foreach {_.cancel()}
        }
      }
      successful({})
    }


    def capture_enabled_get = successful(capture_enabled)


    def getMem(register: MemRegion#ReadableRegister[Int]): Future[Int] =
      mem.read(register)

    def getMemConv[@specialized(Int, Long, Float, Double)  U](bits: MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[U] =
      mem.readConv(bits)

    def getMemConv[@specialized(Int, Long, Float, Double)  U](bits: (Int, Int) => MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(ch: Ch)(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[ChV[U]] =
      ch ftVMap { channel => mem.readConv(chSubst(bits)(channel))}

    def getMemConvFPGA[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(ch: Ch)(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[ChV[U]] =
      ch ftVMap { channel => mem.readConv(chSubstFPGA(bits)(channel))}

    def setMem(register: MemRegion#WriteableRegister[Int], value: Int)(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[Unit] =
      mem.write(register, value)

    def setMemConv[@specialized(Int, Long, Float, Double)  U](bits: MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U], value: U)(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[Unit] =
      mem.writeConv(bits, value)

    def setMemConv[@specialized(Int, Long, Float, Double)  U](bits: (Int, Int) => MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U])(chVals: ChV[U])(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[Unit] =
      chVals map { case (channel, value) => mem.writeConv(chSubst(bits)(channel), value) }

    def setMemConvFPGA[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U])(chVals: ChV[U])(implicit ctx: ExecutionContext, numType: IntegerNumType[Int]): Future[Unit] =
      chVals map { case (channel, value) => mem.writeConv(chSubstFPGA(bits)(channel), value) }


    def startFIFOReadTransfer(ch: Int, bank: Int, from: Int) = {
      import SIS3316Memory.registers.DataTransferCtlReg.Cmd.{Read => fifoReadCmd}
      import SIS3316Memory.dataRegion.{fpgaChMemSpaceSel, fpgaChFIFOAddrOffset}

      val (group, grpCh) = fpgaNumCh(ch)
      mem.sync()
      mem.write(registers.data_transfer_ctrl_reg(group).tiedValue(
        cmd = fifoReadCmd, mem_space_sel = fpgaChMemSpaceSel(grpCh),
        mem_addr = fpgaChFIFOAddrOffset(grpCh, bank).toInt + from
      ))
    }


    def resetFIFO(ch: Int) = {
      import SIS3316Memory.registers.DataTransferCtlReg.Cmd.{Reset => fifoResetCmd}
      val group = fpgaNum(ch)
      mem.sync()
      mem.write(registers.data_transfer_ctrl_reg(group).tiedValue(cmd = fifoResetCmd))
    }


    def readFIFOData(ch: Int, nBytes: Int) = {
      val group = fpgaNum(ch)
      val readAddr = SIS3316Memory.dataRegion.fifo(group).from
      mem.sync()
      mem.readBulk(readAddr, nBytes)
    }


    def event_format_get(ch: Ch) = {
      def getSingle(ch: Int): Future[(Int, EventFormat)] = localExec( async {
        // TODO: Add support for extended sample length
        // TODO: Add support for averaging value data format

        val (group, grpCh) = fpgaNumCh(ch)
        val fpgaRegs = registers.fpga(group)
        val formatConfReg = registers.fpga(group).dataformat_config_reg.ch(grpCh)

        val sel_test_buf = getMemConv(formatConfReg.sel_test_buf)
        val save_maw_test = getMemConv(formatConfReg.save_maw_test)
        val save_energy = getMemConv(formatConfReg.save_energy)
        val save_ft_maw = getMemConv(formatConfReg.save_ft_maw)
        val save_acc_78 = getMemConv(formatConfReg.save_acc_78)
        val save_ph_acc16 = getMemConv(formatConfReg.save_ph_acc16)

        val nSamples = getMemConv(fpgaRegs.raw_data_buffer_config_reg.sample_length)

        val nMAWTestWords = getMemConv(fpgaRegs.maw_test_buffer_config_reg.buffer_len)

        await(Seq(
          sel_test_buf, save_maw_test, save_energy, save_ft_maw, save_acc_78, save_ph_acc16,
          nSamples, nMAWTestWords
        ))

        ch -> EventFormat(
          save_maw_test = if (save_maw_test.v) Some(sel_test_buf.v) else None,
          save_energy = save_energy.v,
          save_ft_maw = save_ft_maw.v,
          save_acc_78 = save_acc_78.v,
          save_ph_acc16 = save_ph_acc16.v,
          nSamples = nSamples.v,
          nMAWTestWords = nMAWTestWords.v
        )
      } (_) )

      ChV.future(for (channel <- ch.toSeq) yield getSingle(channel))
    }


    def sampling_status = localExec( async {
        val acsReg = registers.acquisition_control_status

        val busy = getMemConv(acsReg.smpl_busy)
        val armed = getMemConv(acsReg.smpl_armed)
        val armedBank = getMemConv(acsReg.smpl_armed_bank)

        await(Seq(busy, armed, armedBank))

        SamplingStatus(
         busy = busy.v,
         armed = armed.v,
         armedBank = armedBank.v
        )
    } (_) )


    def newEventsAvail = mem.readConv(registers.acquisition_control_status.mem_addr_thresh)


    def armBank(bank: Int) = {
      mem.sync()
      mem.write(registers.key_disarm_and_arm_bank(bank))
    }


    def swapBanks() = localExec( async {
      mem.sync()
      val samplingBank = getMemConv(registers.fpga(1).actual_sample_address_reg(1).bank)
      await(armBank(if (await(samplingBank) == 1) 2 else 1))
    } (_) )


    def clearAndArm() = {
      armBank(2)
      armBank(1)
    }


    def disarm() = mem.write(registers.key_disarm)


    def dataToRead(ch: Ch) = localExec( async {
      val armedBank = getMemConv(registers.acquisition_control_status.smpl_armed_bank)


      val prevBank = getMemConv(registers.fpga(_).previous_bank_sample_address_reg(_).bank)(ch)
      val prevBankFill = getMemConv(registers.fpga(_).previous_bank_sample_address_reg(_).sample_addr)(ch)
      val format = event_format_get(ch)

      await(Seq(prevBank, prevBankFill, format))

      ch vMap { channel =>
        val bank = prevBank.v(channel)
        assert(bank != armedBank.v)

        DataToRead(
          bank = bank,
          from = 0,
          until = prevBankFill.v(channel),
          format = format.v(channel)
        )
      }
    } (_) )


    def readAllRawEventData(channel: Int) = localExec( async {
      val toRead = await(dataToRead(Ch(channel)))(channel)
      await(readRawEventData(toRead.bank, channel, 0, toRead.until))
    } (_) )


    def readRawEventData(bank: Int, ch: Int, from: Int, nBytes: Int) = localExec( async {
      require((from >= 0) && (nBytes >= 0))

      val until = from + nBytes
      val paddedFrom =  (from / 8 * 8) // Must be a multiple of 64 bit
      val paddedUntil = ((until + 7) / 8 * 8) // Must be a multiple of 64 bit

      require(paddedFrom >= 0)
      require(paddedUntil <= 4 * 0xfffffE)

      resetFIFO(ch)
      startFIFOReadTransfer(ch, bank, paddedFrom)
      val paddedData = await(readFIFOData(ch, paddedUntil - paddedFrom))
      resetFIFO(ch)

      val data = paddedData.drop(from - paddedFrom).take(nBytes)
      assert(data.size == nBytes)
      data
    } (_) )


    var channels_active: Ch = allChannels
    var capture_enabled: Boolean = false
    var run_continous: Boolean = true
    var scheduledPoll: Option[Cancellable] = None
    var dataForReadOut: ChV[DataToRead] = ChV[DataToRead]()


    protected def readOutBankData(): Unit = {
      if (!dataForReadOut.isEmpty) localExec( async {
        val evtData: Future[ChV[ByteString]] = dataForReadOut ftCVVMap { case(channel, toRead) =>
          readRawEventData(toRead.bank, channel, toRead.from, toRead.until - toRead.from)
        }
        await(evtData)
        val chDataIterators: ChV[ByteIterator] = evtData.v map { case (ch, bytes) => (ch, bytes.iterator) }

        val sizes = chDataIterators map { case (ch, it) => (ch, it.clone.size) }
        log.trace(s"Channel data sizes: $sizes")


        dataForReadOut = ChV[DataToRead]()
        if (run_continous) pollNewEvents()
        else capture_enabled = false
      } (_) )
    }


    protected def pollNewEvents(): Unit = {
      if (capture_enabled && dataForReadOut.isEmpty) localExec( async {
        if (await(newEventsAvail)) {
          log.trace("Reading new events")
          await(swapBanks)
          dataForReadOut = await(dataToRead(channels_active))
          readOutBankData()
        } else if (scheduledPoll == None) {
          scheduledPoll = Some(scheduleOnce(100.milliseconds, selfRef, PollNewEvents))
        }
      } (_) )
    }


    protected case object PollNewEvents

    override def receive = extend(super.receive) {
      case PollNewEvents =>
        scheduledPoll = None
        pollNewEvents()
    }
  }


  object SIS3316Impl {
    def fpgaNum(devCh: Int) = (devCh - 1) / 4 + 1
    def fpgaCh(devCh: Int) = (devCh - 1) % 4 + 1
    def fpgaNumCh(devCh: Int) = (fpgaNum(devCh), fpgaCh(devCh))
    def chSubst[T](f: (Int, Int) => T)(ch: Int) = { val (fpgaNum, fpgaCh) = fpgaNumCh(ch); f(fpgaNum, fpgaCh) }
    def chSubstFPGA[T](f: Int => T)(ch: Int) = { val (fpgaNum, fpgaCh) = fpgaNumCh(ch); f(fpgaNum) }

    def getChEvent(it: ByteIterator, encoding: ValEncoding, nSamples: Int, nMAWSamples: Int): dataTypes.RawChEvent = {
      // TODO: Add support for averaging value data format

      import SIS3316Memory.eventFormat._
      import dataTypes._

      implicit val nioByteOrder = encoding.asInstanceOf[ByteOrder].nioByteOrder

      val hdr1 = it.getInt
      val hdr2 = it.getInt

      val chId = evtDataHdr1.ch_id(hdr1)

      val ts_high = evtDataHdr1.timestamp_high(hdr1)
      val ts_low = evtDataHdr2.timestamp_low(hdr2)
      val timestamp = (ts_high.asUnsigned.toLong << 32) | (ts_low.asUnsigned.toLong << 0)

      var evtFlags: Option[EvtFlags] = None
      var accSums: ChV[Int] = ChV[Int]()
      var peakHeight: Option[PSAValue] = None
      var mawValues: Option[MAWValues] = None
      var energyValues: Option[EnergyValues] = None


      if (evtDataHdr1.have_ph_acc16(hdr1)) {
        val ph_word = it.getInt
        val acc1_word = it.getInt
        val acc2_word = it.getInt
        val acc3_word = it.getInt
        val acc4_word = it.getInt
        val acc5_word = it.getInt
        val acc6_word = it.getInt

        peakHeight = Some( PSAValue (
          index = evtDataPeakHeight.peak_heigh_idx(ph_word),
          value = evtDataPeakHeight.peak_heigh_val(ph_word)
        ) )

        evtFlags = Some( EvtFlags (
          overflow = evtDataAccSumG1.overflow_flag(acc1_word),
          underflow = evtDataAccSumG1.underflow_flag(acc1_word),
          repileup = evtDataAccSumG1.repileup_flag(acc1_word),
          pileup = evtDataAccSumG1.pileup_flag(acc1_word)
        ) )

        accSums = accSums ++ Map(
          1 -> evtDataAccSumG1.acc_sum_g1(acc1_word),
          2 -> evtDataAccSum.acc_sum(acc2_word),
          3 -> evtDataAccSum.acc_sum(acc3_word),
          4 -> evtDataAccSum.acc_sum(acc4_word),
          5 -> evtDataAccSum.acc_sum(acc5_word),
          6 -> evtDataAccSum.acc_sum(acc6_word)
        )
      }


      if (evtDataHdr1.have_acc_78(hdr1)) {
        val acc7_word = it.getInt
        val acc8_word = it.getInt

        accSums = accSums ++ Map(
          7 -> evtDataAccSum.acc_sum(acc7_word),
          8 -> evtDataAccSum.acc_sum(acc8_word)
        )
      }


      if (evtDataHdr1.have_ft_maw(hdr1)) {
        val maw_max_word = it.getInt
        val maw_preTrig_word = it.getInt
        val maw_postTrig_word = it.getInt

        mawValues = Some( MAWValues (
          maximum = evtDataMAWValue.maw_val(maw_max_word),
          preTrig = evtDataMAWValue.maw_val(maw_preTrig_word),
          postTrig = evtDataMAWValue.maw_val(maw_postTrig_word)
        ) )
      }


      if (evtDataHdr1.have_energy(hdr1)) {
        val start_energy_word = it.getInt
        val max_energy_word = it.getInt

        energyValues = Some( EnergyValues(
          initial = start_energy_word,
          maximum = max_energy_word
        ) )
      }


      val samples_hdr_word = it.getInt

      require(evtSamplesHdr.const_tag(samples_hdr_word) == 0xE)
      val mawTestFlag = evtSamplesHdr.maw_test_flag(samples_hdr_word)
      val pileupFlag = evtSamplesHdr.any_pileup_flag(samples_hdr_word)
      val nSampleWords = evtSamplesHdr.n_sample_words(samples_hdr_word)

      evtFlags foreach { flags => require(flags.pileup == pileupFlag) }

      def getSamples(nSampleWords: Int): ArrayVec[Int] = {
        val samplesBuilder = ArrayVec.newBuilder[Int]
        samplesBuilder.sizeHint(2 * nSampleWords)
        for (i <- 1 to nSampleWords) {
          val samplesWord = it.getInt
          samplesBuilder += (samplesWord >>>  0) & 0xFFFF
          samplesBuilder += (samplesWord >>> 16) & 0xFFFF
        }
        samplesBuilder.result
      }

      def getMAWSamples(nSampleWords: Int): ArrayVec[Int] = {
        val samplesBuilder = ArrayVec.newBuilder[Int]
        samplesBuilder.sizeHint(nSampleWords)
        for (i <- 1 to nSampleWords) samplesBuilder += it.getInt
        samplesBuilder.result
      }

      require(nSamples == 2 * nSampleWords, s"Expected number of samples ($nSamples) must equal 2x number of sample words ($nSampleWords) in event")
      val samples = getSamples(nSampleWords)

      var mawSamples = ArrayVec[Int]()

      if (mawTestFlag) {
        require(nMAWSamples % 2 == 0)
        mawSamples = getMAWSamples(nMAWSamples)
      }

      RawChEvent(
        chId = chId,
        timestamp = timestamp,
        flags = evtFlags,
        accSums = accSums,
        peakHeight = peakHeight,
        mawValues = mawValues,
        energyValues = energyValues,
        pileupFlag = pileupFlag,
        samples = samples,
        mawSamples = mawSamples
      )
    }
  }
}
