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

import java.util.concurrent.TimeoutException

import akka.actor.{ActorRef, Cancellable}

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

  def event_builder_time_window(): Future[Double]
  def event_builder_time_window(value: Double): Future[Unit]

  def trigger_extern_enabled_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def trigger_extern_enabled_set(chV: ChV[Boolean]): Future[Unit]

  def trigger_intern_gen_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def trigger_intern_gen_set(chV: ChV[Boolean]): Future[Unit]

  def trigger_intern_use_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def trigger_intern_use_set(chV: ChV[Boolean]): Future[Unit]

  def trigger_intern_enabled_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def trigger_intern_enabled_set(chV: ChV[Boolean]): Future[Unit]

  def trigger_intern_feedback_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def trigger_intern_feedback_set(chV: ChV[Boolean]): Future[Unit]

  def trigger_gate_window_length_get(ch: Ch = allChannels): Future[ChV[Int]]
  def trigger_gate_window_length_set(chV: ChV[Int]): Future[Unit]

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

  def input_termination_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def input_termination_set(chV: ChV[Boolean]): Future[Unit]

  def input_gain_get(ch: Ch = allChannels): Future[ChV[Int]]
  def input_gain_set(chV: ChV[Int]): Future[Unit]

  def nsamples_total_get(ch: Ch = allChannels): Future[ChV[Int]]
  def nsamples_total_set(chV: ChV[Int]): Future[Unit]

  def nsamples_start_get(ch: Ch = allChannels): Future[ChV[Int]]
  def nsamples_start_set(chV: ChV[Int]): Future[Unit]

  def nsamples_pretrig_get(ch: Ch = allChannels): Future[ChV[Int]]
  def nsamples_pretrig_set(chV: ChV[Int]): Future[Unit]

  def nmaw_total_get(ch: Ch = allChannels): Future[ChV[Int]]
  def nmaw_total_set(chV: ChV[Int]): Future[Unit]

  def nmaw_pretrig_get(ch: Ch = allChannels): Future[ChV[Int]]
  def nmaw_pretrig_set(chV: ChV[Int]): Future[Unit]

  def acc_length_get(acc: Int)(ch: Ch = allChannels): Future[ChV[Int]]
  def acc_length_set(acc: Int)(chV: ChV[Int]): Future[Unit]

  def acc_start_get(acc: Int)(ch: Ch = allChannels): Future[ChV[Int]]
  def acc_start_set(acc: Int)(chV: ChV[Int]): Future[Unit]

  def bank_fill_threshold_nbytes_get(ch: Ch = allChannels): Future[ChV[Int]]
  def bank_fill_threshold_nbytes_set(chV: ChV[Int]): Future[Unit]

  def bank_fill_threshold_stop_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def bank_fill_threshold_stop_set(chV: ChV[Boolean]): Future[Unit]

  def time_start_get: Future[Double]

  def time_stop_get: Future[Double]

  def forceTrig(): Future[Unit]

  def resetTimestamp(): Future[Unit]

  def startCapture(): Future[Unit]
  def stopCapture(): Future[Unit]

  def capture_enabled_set(value: Boolean): Future[Unit]
  def capture_enabled_get: Future[Boolean]

  def buffer_counter_get: Future[Int]

  def input_readout_enabled_get(ch: Ch = allChannels): Future[ChV[Boolean]]
  def input_readout_enabled_set(chV: ChV[Boolean]): Future[Unit]

  def raw_output_file_basename_get(value: String): Future[String]
  def raw_output_file_basename_set(value: String): Future[Unit]
  def raw_output_file_name_get: Future[String]

  def props_output_file_basename_get(value: String): Future[String]
  def props_output_file_basename_set(value: String): Future[Unit]
  def props_output_file_name_get: Future[String]

  def getMem(register: MemRegion#ReadableRegister[Int]): Future[Int]
  def getMemConv[@specialized(Int, Long, Float, Double)  U](bits: MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(implicit numType: IntegerNumType[Int]): Future[U]
  def getMemConv[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(ch: Ch)(implicit numType: IntegerNumType[Int]): Future[ChV[U]]
  def getMemConv[@specialized(Int, Long, Float, Double)  U](bits: (Int, Int) => MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(ch: Ch)(implicit numType: IntegerNumType[Int]): Future[ChV[U]]
  def getMemConvFPGA[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(ch: Ch)(implicit numType: IntegerNumType[Int]): Future[ChV[U]]

  def setMem(register: MemRegion#WriteableRegister[Int], value: Int)(implicit numType: IntegerNumType[Int]): Future[Unit]
  def setMemConv[@specialized(Int, Long, Float, Double)  U](bits: MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U], value: U)(implicit numType: IntegerNumType[Int]): Future[Unit]
  def setMemConv[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U])(chVals: ChV[U])(implicit numType: IntegerNumType[Int]): Future[Unit]
  def setMemConv[@specialized(Int, Long, Float, Double)  U](bits: (Int, Int) => MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U])(chVals: ChV[U])(implicit numType: IntegerNumType[Int]): Future[Unit]
  def setMemConvFPGA[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U])(chVals: ChV[U])(implicit numType: IntegerNumType[Int]): Future[Unit]

  def startFIFOReadTransfer(ch: Int, bank: Int, from: Int = 0): Future[Unit]
  def resetFIFO(ch: Int): Future[Unit]
  def readFIFOData(ch: Int, nWords: Int): Future[ByteString]

  def event_format_get(ch: Ch = Ch(1 to 16)): Future[ChV[EventFormat]]
  def event_format_set(chV: ChV[EventFormat]): Future[Unit]

  def sampling_status: Future[SamplingStatus]

  def newEventsAvail: Future[Boolean]
  def armBank(bank: Int): Future[Unit]
  def swapBanks(): Future[Unit]
  def clearAndArm(): Future[Unit]
  def disarm(): Future[Unit]
  def dataToRead(ch: Ch = allChannels): Future[ChV[DataToRead]]
  def readAllRawEventData(channel: Int): Future[ByteString]
  def readRawEventData(bank: Int, channel: Int, from: Int, nBytes: Int): Future[ByteString]
  def readRawEventData(channel: Int, dataToRead: DataToRead): Future[ByteString]

  def printBankInfo(): Future[Unit]
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


    object FirmwareType extends Enumeration {
      val FW125 = Value(125)  // SIS3316-125
      val FW250 = Value(250)  // SIS3316-250
    }


    case class RawChEvent(
      chId: Int = 0,
      timestamp: Long = 0,
      flags: Option[EvtFlags] = None,
      accSums: ChV[Int] = ChV[Int](),
      peakHeight: Option[PSAValue] = None,
      trigMAW: Option[MAWValues] = None,
      energy: Option[EnergyValues] = None,
      pileupFlag: Boolean = false,
      samples: ArrayVec[Int] = ArrayVec[Int](),
      mawValues: ArrayVec[Int] = ArrayVec[Int]()
    ) {
      def toProps: Props = {
        Props(
          'channel -> (chId + 1),
          'time -> timestamp,
          'pileup -> pileupFlag,
          'accSum -> Props (
            'i -> PropVal.from(accSums.keys.toArrayVec),
            'v -> PropVal.from(accSums.values.toArrayVec)
          )
        ) ++ (
          energy map { x => (PropKey('energy), PropVal(x.maximum)) }
        ) ++ (
          trigMAW map { x => (PropKey('trigMAW), PropVal(x.maximum)) }
        ) ++ (
          trigMAW map { x => (PropKey('trigMAW), Props (
           'maximum -> x.maximum,
           'preTrig -> x.preTrig,
           'postTrig -> x.postTrig
          ) ) }
        ) ++ (
          peakHeight map { x => (PropKey('peakHeight), Props (
           'index -> x.index,
           'value -> x.value
          ) ) }
        ) ++ (
          flags map { x => (PropKey('flags), Props (
            'overflow -> x.overflow,
            'underflow -> x.underflow,
            'repileup -> x.repileup,
            'pileup -> x.pileup
          ) ) }
        )
      }
    }


    case class RawEvent(
      chId: ChV[Int] = ChV[Int](),
      timestamp: ChV[Long] = ChV[Long](),
      flags: ChV[EvtFlags] = ChV[EvtFlags](),
      accSums: ChV[Int] = ChV[Int](),
      peakHeight: ChV[PSAValue] = ChV[PSAValue](),
      trigMAW: ChV[MAWValues] = ChV[MAWValues](),
      energy: ChV[EnergyValues] = ChV[EnergyValues](),
      pileupFlag: ChV[Boolean] = ChV[Boolean](),
      samples: ChV[ArrayVec[Int]] = ChV[ArrayVec[Int]](),
      mawValues: ChV[ArrayVec[Int]] = ChV[ArrayVec[Int]]()
    )


    case class BankChannelHeaderInfo (
      firmwareType: FirmwareType.Value,
      bufferNo: Int,
      channel: Int,
      nEvents: Int,
      nWordsPerEvent: Int,
      nMAWValues: Int,
      reserved: Int = 0
    ) extends HasByteRep {
      implicit val nioByteOrder = LittleEndian.nioByteOrder

      def putBytes(builder: ByteStringBuilder): Unit = {
        builder.putInt(0xDEADBEEF)
        builder.putInt( firmwareType match {
          case FirmwareType.FW125 => 1
          case FirmwareType.FW250 => 0
        } )
        builder.putInt(bufferNo)
        builder.putInt(channel - 1)
        builder.putInt(nEvents)
        builder.putInt(nWordsPerEvent)
        builder.putInt(nMAWValues)
        builder.putInt(reserved)
      }
    }


    case class EventFormat(
      save_maw_values: Option[Boolean] = None,  // Save (Energy or Trigger) MAW Test Data
      save_energy: Boolean = false,  // Save energy values (at trigger and maximum during trigger window)
      save_ft_maw: Boolean = false,  // Save fast trigger MAW values (max value, value before Trigger, value with Trigger)
      save_acc_78: Boolean = false,  // Save accumulator values for gates 7 and 8
      save_ph_acc16: Boolean = false,  // Save peak height and accumulator values for gates 1,2, ..,6
      nSamples: Int = 0,  // Number of raw samples to save
      nMAWValues: Int = 0  // Number of MAW test data words to save
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
          (if (save_maw_values != None) nMAWValues else 0)

        nEvtWords * sizeOfInt
      }
    }


    case class DataToRead(
      bank: Int = 0,
      from: Int = 0,
      until: Int = 0,
      format: EventFormat = EventFormat()
    ) {
      require(until >= from)
      def nBytes: Int = until - from
      def isEmpty: Boolean = ! (from < until)
    }


    case class SamplingStatus(busy: Boolean = false, armed: Boolean = false, armedBank: Int = 0)

  }


  def impl = { case uri => new SIS3316Impl(uri.toString) }

  class SIS3316Impl(vmeURI: String) extends SIS3316
    with CloseableTAImpl with SyncableImpl with LocalECTypedActorImpl
  {
    import SIS3316Impl._
    import SIS3316Memory.eventFormat._
    import dataTypes._

    implicit def executor = defaultExecContext

    val registers = SIS3316Memory.registers

    val mem = SIS3316Memory(vmeURI, "memory")

    def memory = successful(mem)

    override def sync() = mem.sync()
    override def getSync() = mem.getSync()


    protected val (
      model: String,
      firmwareType: FirmwareType.Value,
      sampleClock: Double,
      bulkReadNIOByteOrder: java.nio.ByteOrder,
      bulkWriteNIOByteOrder: java.nio.ByteOrder
    ) = {
      import daqcore.defaults.defaultTimeout
      val bulkReadEncoding = mem.bulkReadEncoding.get.asInstanceOf[ByteOrder].nioByteOrder
      val bulkWriteEncoding = mem.bulkWriteEncoding.get.asInstanceOf[ByteOrder].nioByteOrder

      val ident = identity.get

      val fwType = ident match {
          case "SIS3316-125" => FirmwareType.FW125
          case "SIS3316-250" => FirmwareType.FW250
          case other => throw new RuntimeException(s"Found unsupported device model $other")
      }

      val smplClk = fwType match {
          case FirmwareType.FW125 => 125E6
          case FirmwareType.FW250 => 250E6
       }

      (ident, fwType, smplClk, bulkReadEncoding, bulkWriteEncoding)
    }


    def identity = {
      val modId = getMemConv(registers.modid.module_id)
      val fwType = getMemConv(registers.fpga(1).firmware_reg.fw_type)
      Future.sequence(Seq(modId, fwType)).map{ case Seq(id, tp) => s"SIS$id-$tp" }
    }

    def serNo = mem.read(registers.serial_number_reg) map { x => x.toString }

    def internalTemperature = getMemConv(registers.internal_temperature_reg.temperature)

    var evtBuilderTimeWindow = 16E-9
    def event_builder_time_window() = successful(evtBuilderTimeWindow)
    def event_builder_time_window(value: Double) = {
      evtBuilderTimeWindow = value
      successful({})
    }


    def trigger_extern_enabled_get(ch: Ch) = getMemConv(registers.fpga(_).event_config_reg.ch(_).ext_trig_en)(ch)
    def trigger_extern_enabled_set(chV: ChV[Boolean]) = setMemConv(registers.fpga(_).event_config_reg.ch(_).ext_trig_en)(chV)

    def trigger_intern_gen_get(ch: Ch) = getMemConv(registers.fpga(_).fir_trigger_threshold_reg(_).trig_en)(ch)
    def trigger_intern_gen_set(chV: ChV[Boolean]) = setMemConv(registers.fpga(_).fir_trigger_threshold_reg(_).trig_en)(chV)

    def trigger_intern_use_get(ch: Ch) = getMemConv(registers.fpga(_).event_config_reg.ch(_).int_trig_en)(ch)
    def trigger_intern_use_set(chV: ChV[Boolean]) = setMemConv(registers.fpga(_).event_config_reg.ch(_).int_trig_en)(chV)


    def trigger_intern_enabled_get(ch: Ch) = {
      // Improve:
      trigger_intern_use_get(ch)
    }


    def trigger_intern_enabled_set(chV: ChV[Boolean]) = {
      Seq(trigger_intern_gen_set(chV), trigger_intern_use_set(chV))
    }


    def trigger_intern_feedback_get(ch: Ch) = getMemConv(registers.internal_trigger_feedback_select_reg.sel_trig(_))(ch)

    def trigger_intern_feedback_set(chV: ChV[Boolean]) = {
      // Enable general internal trigger forwarding if required:
      if ( chV exists {case (ch,v) => v == true} )
        setMemConv(registers.acquisition_control_status.int_trig_to_ext, true)

      // Enable internal trigger forwarding for selected channels:
      setMemConv(registers.internal_trigger_feedback_select_reg.sel_trig(_))(chV)
    }


    def trigger_gate_window_length_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).trigger_gate_window_length_reg.length)(ch)
    def trigger_gate_window_length_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).trigger_gate_window_length_reg.length)(chV)

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

    def input_termination_get(ch: Ch) = getMemConv(registers.fpga(_).analog_ctrl_reg.ch(_).term_dis)(ch) map { _ vMap (a => !a)}
    def input_termination_set(chV: ChV[Boolean]) = setMemConv(registers.fpga(_).analog_ctrl_reg.ch(_).term_dis)(chV vMap (a => !a))

    def input_gain_get(ch: Ch) = getMemConv(registers.fpga(_).analog_ctrl_reg.ch(_).gain_ctrl)(ch)
    def input_gain_set(chV: ChV[Int]) = setMemConv(registers.fpga(_).analog_ctrl_reg.ch(_).gain_ctrl)(chV)

    def nsamples_total_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).raw_data_buffer_config_reg.sample_length)(ch)
    def nsamples_total_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).raw_data_buffer_config_reg.sample_length)(chV)

    def nsamples_start_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).raw_data_buffer_config_reg.start_index)(ch)
    def nsamples_start_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).raw_data_buffer_config_reg.start_index)(chV)

    def nsamples_pretrig_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).pre_trigger_delay_reg.delay)(ch)
    def nsamples_pretrig_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).pre_trigger_delay_reg.delay)(chV)

    def nmaw_total_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).maw_test_buffer_config_reg.buffer_len)(ch)
    def nmaw_total_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).maw_test_buffer_config_reg.buffer_len)(chV)

    def nmaw_pretrig_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).maw_test_buffer_config_reg.pretrig_delay)(ch)
    def nmaw_pretrig_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).maw_test_buffer_config_reg.pretrig_delay)(chV)

    def acc_length_get(acc: Int)(ch: Ch) = getMemConvFPGA(registers.fpga(_).accumulator_config_reg(acc).gate_len)(ch)
    def acc_length_set(acc: Int)(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).accumulator_config_reg(acc).gate_len)(chV)

    def acc_start_get(acc: Int)(ch: Ch) = getMemConvFPGA(registers.fpga(_).accumulator_config_reg(acc).gate_start)(ch)
    def acc_start_set(acc: Int)(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).accumulator_config_reg(acc).gate_start)(chV)

    def bank_fill_threshold_nbytes_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).address_threshold_reg.addr_thresh_value)(ch)
    def bank_fill_threshold_nbytes_set(chV: ChV[Int]) = setMemConvFPGA(registers.fpga(_).address_threshold_reg.addr_thresh_value)(chV)

    def bank_fill_threshold_stop_get(ch: Ch) = getMemConvFPGA(registers.fpga(_).address_threshold_reg.stop_on_thresh)(ch)
    def bank_fill_threshold_stop_set(chV: ChV[Boolean]) = setMemConvFPGA(registers.fpga(_).address_threshold_reg.stop_on_thresh)(chV)

    def time_start_get = successful(time_start)

    def time_stop_get = successful(time_stop)


    def forceTrig() = {
      mem.sync()
      mem.write(registers.key_trigger)
    }


    def resetTimestamp() = {
      mem.sync()
      mem.write(registers.key_timestamp_clear)
    }


    def startCapture() = {
      if (capture_enabled) {
        assert(capture_active)
      } else {
        if (capture_active) {
          throw new IllegalArgumentException("Can't start capture while stopping")
        } else {
          log.debug("Starting capture")

          disarm()
          resetTimestamp()
          buffer_counter = 0

          capture_enabled = true
          capture_active = true
          time_start = currentTime

          clearAndArm()
          openOutput()
          pollNewEvents()
        }
      }
      successful({})
    }


    protected var stopPromises: List[Promise[Unit]] = Nil

    def stopCapture() = {
      if (!capture_active) {
        assert (!capture_enabled)
        successful({})
      } else {
        if (capture_enabled) {
          log.debug("Stopping capture")
          capture_enabled = false
          if (!readout_active) swapBanksAndReadOut()
          else readOtherBank = true
        }

        assert(captureIsStopping)

        val promise = Promise[Unit]()
        stopPromises = promise :: stopPromises
        promise.future
      }
    }


    protected def finishCapture() = {
      log.debug("Finishing capture")

      time_stop = currentTime
      capture_active = false
      capture_enabled = false

      cancelEventPoll()
      closeOutput()
      disarm()

      stopPromises.reverse foreach { _ success {}}
      stopPromises = Nil
    }


    def capture_enabled_set(value: Boolean) = value match {
      case true => startCapture()
      case false => stopCapture()
    }

    def capture_enabled_get = successful(capture_enabled)

    def buffer_counter_get = successful(buffer_counter)


    protected var raw_output_file_basename: String = ""
    protected var raw_output_file_name: String = ""
    protected var rawOutputStream: Option[OutputStreamWriter] = None

    protected var props_output_file_basename: String = ""
    protected var props_output_file_name: String = ""
    protected var propsOutputStream: Option[OutputStreamWriter] = None


    protected def openOutput(): Unit = {
      assert(rawOutputStream.isEmpty)
      assert(!readout_active)

      if (! raw_output_file_basename.isEmpty) {
        val timeStamp = isoTimeStamp(time_start)
        raw_output_file_name = s"${raw_output_file_basename}-${timeStamp}-raw.dat"
        rawOutputStream = Some(OutputStreamWriter(raw_output_file_name, raw_output_file_name + ".tmp", "raw-data-writer"))
      }

      if (! props_output_file_basename.isEmpty) {
        val timeStamp = isoTimeStamp(time_start)
        props_output_file_name = s"${props_output_file_basename}-${timeStamp}-props.json"
        propsOutputStream = Some(OutputStreamWriter(props_output_file_name, props_output_file_name + ".tmp", "props-data-writer"))
      }
    }


    protected def closeOutput(): Unit = {
      raw_output_file_name = ""
      if (rawOutputStream != None) {
        rawOutputStream.get.close()
        rawOutputStream = None
      }

      props_output_file_name = ""
      if (propsOutputStream != None) {
        propsOutputStream.get.close()
        propsOutputStream = None
      }
    }


    def input_readout_enabled_get(ch: Ch = allChannels) = successful(input_readout_enabled)
    def input_readout_enabled_set(chV: ChV[Boolean]) = successful(input_readout_enabled = chV)


    def raw_output_file_basename_get(value: String) = successful(raw_output_file_basename)

    def raw_output_file_basename_set(value: String) = {
      raw_output_file_basename = value
      successful({})
    }

    def raw_output_file_name_get = successful(raw_output_file_name)


    def props_output_file_basename_get(value: String) = successful(props_output_file_basename)

    def props_output_file_basename_set(value: String) = {
      props_output_file_basename = value
      successful({})
    }

    def props_output_file_name_get = successful(props_output_file_name)


    def getMem(register: MemRegion#ReadableRegister[Int]): Future[Int] =
      mem.read(register)

    def getMemConv[@specialized(Int, Long, Float, Double)  U](bits: MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(implicit numType: IntegerNumType[Int]): Future[U] =
      mem.readConv(bits)

    def getMemConv[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(ch: Ch)(implicit numType: IntegerNumType[Int]): Future[ChV[U]] =
      ch ftVMap { channel => mem.readConv(bits(channel))}

    def getMemConv[@specialized(Int, Long, Float, Double)  U](bits: (Int, Int) => MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(ch: Ch)(implicit numType: IntegerNumType[Int]): Future[ChV[U]] =
      ch ftVMap { channel => mem.readConv(chSubst(bits)(channel))}

    def getMemConvFPGA[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#ReadableRegister[Int]#ReadableBitSelection[U])(ch: Ch)(implicit numType: IntegerNumType[Int]): Future[ChV[U]] =
      ch ftVMap { channel => mem.readConv(chSubstFPGA(bits)(channel))}

    def setMem(register: MemRegion#WriteableRegister[Int], value: Int)(implicit numType: IntegerNumType[Int]): Future[Unit] =
      mem.write(register, value)

    def setMemConv[@specialized(Int, Long, Float, Double)  U](bits: MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U], value: U)(implicit numType: IntegerNumType[Int]): Future[Unit] =
      mem.writeConv(bits, value)

    def setMemConv[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U])(chVals: ChV[U])(implicit numType: IntegerNumType[Int]): Future[Unit] =
      chVals map { case (channel, value) => mem.writeConv(bits(channel), value) }

    def setMemConv[@specialized(Int, Long, Float, Double)  U](bits: (Int, Int) => MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U])(chVals: ChV[U])(implicit numType: IntegerNumType[Int]): Future[Unit] =
      chVals map { case (channel, value) => mem.writeConv(chSubst(bits)(channel), value) }

    def setMemConvFPGA[@specialized(Int, Long, Float, Double)  U](bits: Int => MemRegion#PartiallyWriteableRegister[Int]#WriteableBitSelection[U])(chVals: ChV[U])(implicit numType: IntegerNumType[Int]): Future[Unit] =
      chVals map { case (channel, value) => mem.writeConv(chSubstFPGA(bits)(channel), value) }


    def startFIFOReadTransfer(ch: Int, bank: Int, from: Int) = {
      import SIS3316Memory.registers.DataTransferCtlReg.Cmd.{Read => fifoReadCmd}
      import SIS3316Memory.dataRegion.{fpgaChMemSpaceSel, fpgaChFIFOAddrOffset}

      require(from % 4 == 0)
      require(from < 0x10000000)
      val fromWord = from / 4

      val (group, grpCh) = fpgaNumCh(ch)
      mem.sync()
      mem.write(registers.data_transfer_ctrl_reg(group).tiedValue(
        cmd = fifoReadCmd, mem_space_sel = fpgaChMemSpaceSel(grpCh),
        mem_addr = fpgaChFIFOAddrOffset(grpCh, bank).toInt + fromWord
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
        val save_maw_values = getMemConv(formatConfReg.save_maw_values)
        val save_energy = getMemConv(formatConfReg.save_energy)
        val save_ft_maw = getMemConv(formatConfReg.save_ft_maw)
        val save_acc_78 = getMemConv(formatConfReg.save_acc_78)
        val save_ph_acc16 = getMemConv(formatConfReg.save_ph_acc16)

        val nSamples = getMemConv(fpgaRegs.raw_data_buffer_config_reg.sample_length)

        val nMAWValues = getMemConv(fpgaRegs.maw_test_buffer_config_reg.buffer_len)

        await(Seq(
          sel_test_buf, save_maw_values, save_energy, save_ft_maw, save_acc_78, save_ph_acc16,
          nSamples, nMAWValues
        ))

        ch -> EventFormat(
          save_maw_values = if (save_maw_values.v) Some(sel_test_buf.v) else None,
          save_energy = save_energy.v,
          save_ft_maw = save_ft_maw.v,
          save_acc_78 = save_acc_78.v,
          save_ph_acc16 = save_ph_acc16.v,
          nSamples = nSamples.v,
          nMAWValues = nMAWValues.v
        )
      } (_) )

      ChV.future(for (channel <- ch.toSeq) yield getSingle(channel))
    }


    def event_format_set(chV: ChV[EventFormat]) = {
      def setSingle(ch: Int, format: EventFormat): Future[Unit] = {
        // TODO: Add support for extended sample length
        // TODO: Add support for averaging value data format

        val (group, grpCh) = fpgaNumCh(ch)
        val fpgaRegs = registers.fpga(group)
        val formatConfReg = registers.fpga(group).dataformat_config_reg.ch(grpCh)

        Seq(
          setMemConv(formatConfReg.sel_test_buf, format.save_maw_values.getOrElse(false)),
          setMemConv(formatConfReg.save_maw_values, format.save_maw_values != None),
          setMemConv(formatConfReg.save_energy, format.save_energy),
          setMemConv(formatConfReg.save_ft_maw, format.save_ft_maw),
          setMemConv(formatConfReg.save_acc_78, format.save_acc_78),
          setMemConv(formatConfReg.save_ph_acc16, format.save_ph_acc16),
          setMemConv(fpgaRegs.raw_data_buffer_config_reg.sample_length, format.nSamples),
          setMemConv(fpgaRegs.maw_test_buffer_config_reg.buffer_len, format.nMAWValues)
        )
      }

      for ((channel, format) <- chV.toSeq) yield setSingle(channel, format)
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


    def disarm() = {
      mem.sync()
      mem.write(registers.key_disarm)
    }


    def dataToRead(ch: Ch) = localExec( async {
      val armedBank = getMemConv(registers.acquisition_control_status.smpl_armed_bank)
      val prevBank = getMemConv(registers.fpga(_).previous_bank_sample_address_reg(_).bank)(ch)
      val prevBankFill = getMemConv(registers.fpga(_).previous_bank_sample_address_reg(_).sample_addr)(ch)
      val format = event_format_get(ch)

      await(Seq(armedBank, prevBank, prevBankFill, format))

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


    protected def readChunkSize(toRead: DataToRead): Int = {
      val nBytesPerEvent = toRead.format.rawEventDataSize
      val maxChunkSize = 5 * 1024 * 1024
      val matchedChunkSize = maxChunkSize / nBytesPerEvent * nBytesPerEvent
      Math.min(toRead.nBytes, Math.max(nBytesPerEvent, matchedChunkSize))
    }


    protected def splitToRead(toRead: DataToRead): (DataToRead, DataToRead) = {
      if (toRead.isEmpty) {
        (toRead, toRead)
      } else {
        val until = toRead.from + readChunkSize(toRead)
        (toRead.copy(until = until), toRead.copy(from = until))
      }
    }


    protected def chunkStream(toRead: DataToRead): Stream[DataToRead] = {
      if (!toRead.isEmpty) {
        val (next, rest) = splitToRead(toRead)
        Stream.cons(next, chunkStream(rest))
      } else Stream.empty
    }


    def readRawEventData(bank: Int, channel: Int, from: Int, nBytes: Int) = {
      require(nBytes >= 0)
      if (nBytes == 0) successful(ByteString.empty)
      else localExec( async {
        require((from >= 0) && (nBytes >= 0))

        val until = from + nBytes
        val paddedFrom =  (from / 8 * 8) // Must be a multiple of 64 bit
        val paddedUntil = ((until + 7) / 8 * 8) // Must be a multiple of 64 bit

        require(paddedFrom >= 0)
        require(paddedUntil <= 4 * 0xfffffE)

        var nTry = 0
        var paddedData = ByteString.empty

        while (paddedData.isEmpty) {
          nTry = nTry + 1
          if (nTry >= 5) throw new RuntimeException(s"Retry limit reached for reading ${phex(nBytes)} bytes of event data from bank $bank, channel $channel, starting at ${phex(from)}")
          if (nTry >= 2) log.debug(s"Trying again to read ${phex(nBytes)} bytes of event data from bank $bank, channel $channel, starting at ${phex(from)}, try no $nTry")
          resetFIFO(channel)
          await(startFIFOReadTransfer(channel, bank, paddedFrom))
          val readResult = readFIFOData(channel, paddedUntil - paddedFrom)
          paddedData = await(readResult.recover{case timeout: TimeoutException => ByteString.empty})
          resetFIFO(channel)
        }

        val data = paddedData.drop(from - paddedFrom).take(nBytes)
        assert(data.size == nBytes)
        data
      } (_) )
    }


    def readRawEventData(channel: Int, dataToRead: DataToRead) =
      readRawEventData(dataToRead.bank, channel, dataToRead.from, dataToRead.nBytes)


    def printBankInfo() = {
      localExec( async {
        val chIterator = allChannels.iterator
        while (chIterator.hasNext) {
          val ch = chIterator.next

          val (group, grpCh) = fpgaNumCh(ch)
          val fpgaRegs = registers.fpga(group)

          val eventSize = await(event_format_get(ch to ch)).apply(ch).rawEventDataSize

          val samplingBank = await(mem.readConv(fpgaRegs.actual_sample_address_reg(grpCh).bank))
          val samplingBankFillNBytes = await(mem.readConv(fpgaRegs.actual_sample_address_reg(grpCh).sample_addr))
          val readoutBank = await(mem.readConv(fpgaRegs.previous_bank_sample_address_reg(grpCh).bank))
          val readoutFillNBytes = await(mem.readConv(fpgaRegs.previous_bank_sample_address_reg(grpCh).sample_addr))

          val samplingNEvents = samplingBankFillNBytes.toDouble / eventSize
          val readOutNEvents = readoutFillNBytes.toDouble / eventSize

          println(s"Channel $ch: Bank $samplingBank armed: bank $samplingBankFillNBytes bytes ($samplingNEvents events), bank $readoutBank readout: $readoutFillNBytes bytes ($readOutNEvents events)")  
        }
        {}
      } (_) )
    }


    protected var time_start: Double = 0
    protected var time_stop: Double = 0
    protected var input_readout_enabled: ChV[Boolean] = allChannels --> true
    protected var capture_enabled: Boolean = false
    protected var capture_active: Boolean = false
    protected var readout_active: Boolean = false
    protected var buffer_counter: Int = 0
    protected var run_continous: Boolean = true
    protected var scheduledPoll: Option[Cancellable] = None

    protected var readOtherBank = false
    protected def captureIsStopping = capture_active && !capture_enabled

    protected def channelsToRead: Ch =
      input_readout_enabled.collect{ case (channel, true) => channel }(breakOut)


    protected def swapBanksAndReadOut(): Unit = {
      assert(capture_active)

      readout_active = true
      log.trace("Starting data read-out")

      localExec( async {
        assert(capture_active)
        await(swapBanks)
        assert(capture_active)

        buffer_counter = buffer_counter + 1
        val dataAvail = await(dataToRead(channelsToRead))

        assert( dataAvail forall { case (channel, toRead) =>
          toRead.nBytes % toRead.format.rawEventDataSize == 0
        } )

        val dataNotEmpty = dataAvail exists{ case (channel, toRead) => toRead.nBytes > 0 }
        if (dataNotEmpty) {
          if (propsOutputStream != None) await(readOutBankDataEvt(dataAvail))
          if (rawOutputStream != None) await(readOutBankDataRaw(dataAvail))
        }

        log.trace("Finishing data read-out")

        if (readOtherBank) {
          readOtherBank = false
          assert(captureIsStopping)
          log.trace("Capture stopping, reading other bank too")
          swapBanksAndReadOut()
        } else {
          readout_active = false
          if (capture_enabled && run_continous) pollNewEvents()
          else finishCapture()
        }
      } (_) )
    }


    // Sorted event read out
    protected def readOutBankDataEvt(dataAvail: ChV[DataToRead]): Future[Unit] = {
      log.trace("Starting sorted event data read-out")
      assert(propsOutputStream != None)
      val stringCodec = StringLineCodec(LineCodec.LF, "UTF-8")

      class ChDataHandler(val channel: Int, val toRead: DataToRead) {
        val dataAvailThreshold = {
          val nBytesPerEvent = toRead.format.rawEventDataSize
          val defaultThreshold = 1 * 1024 * 1024
          defaultThreshold / nBytesPerEvent * nBytesPerEvent
        }

        val evtIterator = new ChEventIterator(toRead.format, bulkReadNIOByteOrder)
        var restToRead = toRead
        var nextRawChunk = Option.empty[Future[ByteString]]

        def moreDataToRead: Boolean = !restToRead.isEmpty

        def readingMoreData: Boolean = !nextRawChunk.isEmpty

        def newDataAvailable: Boolean = !nextRawChunk.isEmpty && nextRawChunk.get.isCompleted

        def futureNewData: Future[ByteString] = {
          require(readingMoreData)
          nextRawChunk.get
        }

        def readMoreData(): Unit = {
          require(!readingMoreData && moreDataToRead)
          val (chunkToRead, rest) = splitToRead(restToRead)
          log.trace(s"Reading next ${chunkToRead.nBytes} bytes from channel ${channel}, ${rest.nBytes} remain")
          restToRead = rest
          val futureNextChunk = readRawEventData(channel, chunkToRead)
          nextRawChunk = Some(futureNextChunk)
        } ensuring (readingMoreData)

        def readMoreDataIfNecessary(): Unit = {
          if (readingMoreData) {
            if (newDataAvailable) useNewData()
          } else {
            if ((evtIterator.dataAvail < dataAvailThreshold) && moreDataToRead)
              readMoreData()
          }
        } ensuring (evtIterator.hasNext || readingMoreData || isFinished)

        def useNewData(): Unit = {
          require(newDataAvailable)
          val chunk = nextRawChunk.get.v
          log.trace(s"Refilling raw buffer for channel ${channel} with ${chunk.size} bytes")
          evtIterator.addData(chunk)
          nextRawChunk = None
        } ensuring (!readingMoreData && evtIterator.hasNext)

        def isFinished = !evtIterator.hasNext && !moreDataToRead && !readingMoreData
      }


      localExec( async {
        var channelHandlers: Array[ChDataHandler] = dataAvail.filter{! _._2.isEmpty}.map{
          case (channel, toRead) => new ChDataHandler(channel, toRead)
        }(breakOut)

        log.trace(s"Number of channels to read out: ${channelHandlers.size}")

        while (!channelHandlers.isEmpty) {
          log.trace(s"Number of channels in outer read-out loop: ${channelHandlers.size}")
          assert(channelHandlers forall { !_.isFinished })


          // Sequential read-out of all data per channel, instead of interleaved read-out (see below):
          var handersToRead = channelHandlers.toList
          while (!handersToRead.isEmpty) {
            val handler = handersToRead.head
            assert(!handler.readingMoreData)
            if (handler.moreDataToRead) {
              handler.readMoreData()
              await(handler.futureNewData)
              handler.useNewData()
              assert(!handler.readingMoreData)
            } else {
              handersToRead = handersToRead.tail
            }
          }


          /*
          // Interleaved, on-demand read-out of channels doesn't work yet, results in corrupt raw data:

          channelHandlers foreach { _.readMoreDataIfNecessary() }
          val readingHandlers = channelHandlers.filter(_.readingMoreData)
          val futureData: List[Future[ByteString]] = readingHandlers.map{_.futureNewData}(breakOut)
          if (!futureData.isEmpty) {
            await(Future.sequence(futureData))
          }
          readingHandlers foreach { _.useNewData() }
          assert(channelHandlers forall { _.evtIterator.hasNext })
          */


          var someChNeedsMoreData = false
          while (!channelHandlers.isEmpty && !someChNeedsMoreData) {
            scala.util.Sorting.stableSort[ChDataHandler, Long](channelHandlers, _.evtIterator.head.timestamp)

            val t0 = channelHandlers.head.evtIterator.head.timestamp
            val lastIdxOfEvent = channelHandlers lastIndexWhere {
              _.evtIterator.head.timestamp - t0 <= evtBuilderTimeWindow
            }

            var someChIsFinished = false

            for (i <- 0 to lastIdxOfEvent ) {
              val handler = channelHandlers(i)
              assert(handler.evtIterator.hasNext)

              val chEvent = handler.evtIterator.next()
              val rawProps = chEvent.toProps;
              val convProps = rawProps + ('time -> rawProps('time).asDouble / sampleClock)
              propsOutputStream.get.send(convProps.toJSON, stringCodec.enc)

              handler.readMoreDataIfNecessary()
              if (handler.isFinished) someChIsFinished = true
              else if (!handler.evtIterator.hasNext) someChNeedsMoreData = true
            }

            if (someChIsFinished) channelHandlers = channelHandlers filter { ! _.isFinished }
          }
        }
      } (_) )
    }


    // Raw bank-data read out (Struck raw format)
    protected def readOutBankDataRaw(dataAvail: ChV[DataToRead]): Future[Unit] = {
      log.trace("Starting unsorted raw data read-out")
      assert(bulkReadNIOByteOrder == LittleEndian.nioByteOrder)
      assert(rawOutputStream != None)

      var dataRemaining: ChV[DataToRead] = dataAvail

      localExec( async {
        val availIterator = dataAvail.iterator
        while (availIterator.hasNext) {
          val (channel, toRead) = availIterator.next

          val nBytesPerEvent = toRead.format.rawEventDataSize
          assert(nBytesPerEvent % 4 == 0)

          val headerInfo = BankChannelHeaderInfo (
            firmwareType = firmwareType,
            bufferNo = buffer_counter - 1,
            channel =  channel,
            nEvents = toRead.nBytes / nBytesPerEvent,
            nWordsPerEvent = nBytesPerEvent / 4,
            nMAWValues = toRead.format.nMAWValues,
            reserved = 0
          )

          rawOutputStream.get.send(headerInfo.getBytes)

          val chunksIterator = chunkStream(toRead).iterator
          while (chunksIterator.hasNext) {
            val chunkToRead = chunksIterator.next
            val data = await(readRawEventData(channel, chunkToRead))
            rawOutputStream.get.send(data)
          }
        }
      } (_) )
    }


    protected def pollNewEvents(): Unit = {
      localExec( async {
        assert(capture_active)
        val avail = await(newEventsAvail)
        if (!readout_active) {
          if (capture_enabled) {
            if (avail) swapBanksAndReadOut()
            else scheduleEventPoll()
          } else {
            log.debug("Capture disabled, aborting event poll")
          }
        } else {
          log.debug("Readout active, aborting event poll")
        }
      } (_) )
    }


    protected def scheduleEventPoll(): Unit = {
      assert(scheduledPoll == None)
      scheduledPoll = Some(scheduleOnce(100.milliseconds, selfRef, PollNewEvents))
    }

    protected def cancelEventPoll(): Unit = if (scheduledPoll != None) {
      scheduledPoll.get.cancel()
      scheduledPoll = None
    }


    protected case object PollNewEvents

    override def receive = extend(super.receive) {
      case PollNewEvents => if (capture_active) {
        scheduledPoll = None
        pollNewEvents()
      } else {
        // Remnant message, ignore
        assert(scheduledPoll == None)
      }
    }
  }


  object SIS3316Impl {
    def fpgaNum(devCh: Int) = (devCh - 1) / 4 + 1
    def fpgaCh(devCh: Int) = (devCh - 1) % 4 + 1
    def fpgaNumCh(devCh: Int) = (fpgaNum(devCh), fpgaCh(devCh))
    def chSubst[T](f: (Int, Int) => T)(ch: Int) = { val (fpgaNum, fpgaCh) = fpgaNumCh(ch); f(fpgaNum, fpgaCh) }
    def chSubstFPGA[T](f: Int => T)(ch: Int) = { val (fpgaNum, fpgaCh) = fpgaNumCh(ch); f(fpgaNum) }

    def getChEvent(it: ByteIterator, byteOrder: java.nio.ByteOrder, nSamples: Int, nMAWSamples: Int): dataTypes.RawChEvent = {
      // TODO: Add support for averaging value data format

      import SIS3316Memory.eventFormat._
      import dataTypes._

      implicit val nioByteOrder = byteOrder

      val hdr1 = it.getInt
      val hdr2 = it.getInt

      val chId = evtDataHdr1.ch_id(hdr1)

      val ts_high = evtDataHdr1.timestamp_high(hdr1)
      val ts_low = evtDataHdr2.timestamp_low(hdr2)
      val timestamp = (ts_high.asUnsigned.toLong << 32) | (ts_low.asUnsigned.toLong << 0)

      var evtFlags: Option[EvtFlags] = None
      var accSums: ChV[Int] = ChV[Int]()
      var peakHeight: Option[PSAValue] = None
      var trigMAW: Option[MAWValues] = None
      var energy: Option[EnergyValues] = None


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

        trigMAW = Some( MAWValues (
          maximum = evtDataMAWValue.maw_val(maw_max_word),
          preTrig = evtDataMAWValue.maw_val(maw_preTrig_word),
          postTrig = evtDataMAWValue.maw_val(maw_postTrig_word)
        ) )
      }


      if (evtDataHdr1.have_energy(hdr1)) {
        val start_energy_word = it.getInt
        val max_energy_word = it.getInt

        energy = Some( EnergyValues(
          initial = start_energy_word,
          maximum = max_energy_word
        ) )
      }


      val samples_hdr_word = it.getInt

      require(evtSamplesHdr.const_tag(samples_hdr_word) == 0xE)
      val mawTestFlag = evtSamplesHdr.maw_test_flag(samples_hdr_word)
      val pileupFlag = evtSamplesHdr.any_pileup_flag(samples_hdr_word)
      val nSampleWords = evtSamplesHdr.n_sample_words(samples_hdr_word)

      // evtFlags foreach { flags => require( pileupFlag == (flags.pileup || flags.repileup) ) }

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

      var mawValues = ArrayVec[Int]()

      if (mawTestFlag) {
        require(nMAWSamples % 2 == 0)
        mawValues = getMAWSamples(nMAWSamples)
      }

      RawChEvent(
        chId = chId,
        timestamp = timestamp,
        flags = evtFlags,
        accSums = accSums,
        peakHeight = peakHeight,
        trigMAW = trigMAW,
        energy = energy,
        pileupFlag = pileupFlag,
        samples = samples,
        mawValues = mawValues
      )
    }


    class ChEventIterator(
      eventFormat: dataTypes.EventFormat,
      val bulkReadNIOByteOrder: java.nio.ByteOrder,
      data: ByteIterator = ByteString().iterator
    ) extends BufferedIterator[dataTypes.RawChEvent] {

      def hasNext: Boolean = !currentHead.isEmpty || rawData.hasNext

      protected var rawData = data
      protected var currentHead: Option[dataTypes.RawChEvent] = None

      def head: dataTypes.RawChEvent = {
        currentHead match {
          case Some(chEvent) =>
            chEvent
          case None =>
            if (rawData.hasNext) {
              val nextEvent = getChEvent(rawData, bulkReadNIOByteOrder, eventFormat.nSamples, eventFormat.nMAWValues)
              currentHead = Some(nextEvent)
              nextEvent
            } else {
              Iterator.empty.next()
            }
        }
      }


      def next(): dataTypes.RawChEvent = {
        val result = head
        currentHead = None
        result
      }


      def addData(data: ByteIterator): Unit = { rawData = rawData ++ data }
      def addData(data: ByteString): Unit = addData(data.iterator)

      def dataAvail = rawData.len
    }


    // Testing functions:

    import daqcore.util.fileops._
    import java.io.File

    def readData(adc: SIS3316, ch: Int): ByteIterator = {
      import daqcore.defaults._
      val mem = adc.memory.get
      val registers = SIS3316Memory.registers

      val (group, grpCh) = fpgaNumCh(ch)
      val fpgaRegs = registers.fpga(group)

      val readByteOrder = mem.bulkReadEncoding.get.asInstanceOf[ByteOrder].nioByteOrder
      val nSamples = mem.readConv(fpgaRegs.raw_data_buffer_config_reg.sample_length).get
      val nMAWSamples = mem.readConv(fpgaRegs.maw_test_buffer_config_reg.buffer_len).get

      val data = adc.readAllRawEventData(ch).get
      println(s"Data size: ${data.size} bytes")
      data.iterator
    }


    def readFullChBankData(adc: SIS3316, ch: Int, bank: Int): ByteIterator = {
      import daqcore.defaults._
      val mem = adc.memory.get
      val registers = SIS3316Memory.registers
      val (group, grpCh) = fpgaNumCh(ch)
      val fpgaRegs = registers.fpga(group)

      val readSize = 64 * 1024 * 1024
      adc.resetFIFO(ch)
      adc.startFIFOReadTransfer(ch, bank)
      val data = adc.readFIFOData(ch, readSize).get
      data.iterator
    }


    def nextChEvent(adc: SIS3316, ch: Int, it: ByteIterator, sampleFile: File = currDir / "sample-values.txt", mawFile: File = currDir / "maw-values.txt") = {
      import daqcore.defaults._
      val mem = adc.memory.get
      val registers = SIS3316Memory.registers
      val (group, grpCh) = fpgaNumCh(ch)
      val fpgaRegs = registers.fpga(group)

      val readByteOrder = mem.bulkReadEncoding.get.asInstanceOf[ByteOrder].nioByteOrder
      val nSamples = mem.readConv(fpgaRegs.raw_data_buffer_config_reg.sample_length).get
      val nMAWSamples = mem.readConv(fpgaRegs.maw_test_buffer_config_reg.buffer_len).get

      val chEvent = getChEvent(it, readByteOrder, nSamples, nMAWSamples)
      sampleFile write (chEvent.samples map { _ - (1 << 13) } mkString "\n")
      mawFile write (chEvent.mawValues map { _ - (1 << 27) } mkString "\n")
      chEvent
    }


    def shortCapture(adc: SIS3316, channel: Int) = {
      import daqcore.defaults._

      adc.trigger_intern_enabled_set(dataTypes.allChannels --> false)
      adc.trigger_intern_enabled_set(Ch(channel) --> true)

      adc.getSync().get

      adc.resetTimestamp()
      adc.clearAndArm.get
      Thread.sleep(1000)

      adc.swapBanks().get
      adc.printBankInfo().get

      val data = readData(adc, channel)
      val chEvent = if (data.hasNext) nextChEvent(adc, channel, data) else dataTypes.RawChEvent()
      println(chEvent.toProps)
      chEvent
    }


    def testTriggerMAW(adc: SIS3316, channel: Int, samplesPretrig: Int = -1, mawPretrig: Int = -1, peakTime: Int = -1, gapTime: Int = -1, threshold: Double = -1) {
      import daqcore.defaults._

      if (samplesPretrig >= 0) adc.nsamples_pretrig_set(channel --> samplesPretrig)
      if (mawPretrig >= 0) adc.nmaw_pretrig_set(channel --> mawPretrig)

      if (gapTime >= 0) adc.trigger_gapTime_set(channel --> gapTime)
      if (peakTime >= 0) adc.trigger_peakTime_set(channel --> peakTime)

      adc.getSync.get
      if (threshold >= 0) {
        val peakTime = adc.trigger_peakTime_get(Ch(channel)).get.apply(channel)
        adc.trigger_threshold_set(channel --> (threshold).toInt)
      }

      adc.disarm.get
      val newEventFormat = adc.event_format_get(Ch(channel)).get.apply(channel).copy(save_maw_values = Some(false))
      adc.event_format_set(channel --> newEventFormat)

      shortCapture(adc, channel)
    }


    def testEnergyMAW(adc: SIS3316, channel: Int, samplesPretrig: Int = -1, mawPretrig: Int = -1, tauTable: Int = -1, tauFactor: Int = -1) {
      import daqcore.defaults._

      if (samplesPretrig >= 0) adc.nsamples_pretrig_set(channel --> samplesPretrig)
      if (mawPretrig >= 0) adc.nmaw_pretrig_set(channel --> mawPretrig)
      if (tauTable >= 0) adc.energy_tau_table_set(channel --> tauTable)
      if (tauFactor >= 0) adc.energy_tau_factor_set(channel --> tauFactor)

      adc.disarm.get
      val newEventFormat = adc.event_format_get(Ch(channel)).get.apply(channel).copy(save_maw_values = Some(true))
      adc.event_format_set(channel --> newEventFormat)

      shortCapture(adc, channel)
    }


    def writeRawChDataToFile(adc: SIS3316, data: ByteIterator): Unit = {
      val array = Array.ofDim[Byte](data.clone.size)
      data.clone.copyToArray(array)
      (currDir / "ch-raw-data.bin") writeBytes array
    }
  }
}
