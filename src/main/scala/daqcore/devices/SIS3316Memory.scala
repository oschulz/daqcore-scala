// Copyright (C) 2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

// This file contains register names and descriptions from the SIS3316
// Manual, copyright by Struck Innovative Systeme, reproduced here with
// permission of SIS.

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
  import MemRegion.MemAddress

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

        registerCache = MemValues[Address, Int]() // clear register cache

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



  case object BCDConv extends ValueConv[Int, Int] with ValueRevConv.SimpleUnapplyConv[Int, Int] {
    def apply(raw: Int) = "%x".format(raw).toInt

    def applyRev(conv: Int) = Integer.parseInt(conv.toString, 16)
  }



  object registers extends MemRegion(0x000000, 0x005000) {
     object CfdCtrl extends Enumeration {
      val CFDDisabled     = Value(0)  // CFD function disabled
      val CFDDisabled_alt = Value(1)  // CFD function disabled
      val CFDZeroCross    = Value(2)  // CFD function enabled with Zero crossing
      val CDF50Percent    = Value(3)  // CFD function enabled with 50 percent
    }


    // VME interface registers
    // -----------------------

    /** Control/Status Register */
    object control_status extends JKRegister[Int](0x00) {
      val reboot_fpgas      = JKBit(15)  // Status Reboot FPGAs

      class LEDBits(offset: Int) extends SubRegister {
        val mode              = JKBit(offset + 4)  // Application Mode
        val state             = JKBit(offset + 0)  // on/off
      }

      val led2 = new LEDBits(2)  // Status LED 2 bits
      val led1 = new LEDBits(1)  // Status LED 1 bits
      val ledU = new LEDBits(0)  // Status LED U bits
    }

    /** Module Id. and Firmware Revision Register */
    object modid extends RORegister[Int](0x04) {
      val module_id         = ROBits(16 to 31) withConv BCDConv  // Module Id. (BCD)
      val major_revision    = ROBits( 8 to 15)  // Major Revision (BCD)
      val minor_revision    = ROBits( 0 to  7)  // Minor Revision (BCD)
    }

    // object udp_prot_configuration extends RWRegister[Int](0x08) {}  // UDP Protocol Configuration Register

    // object irq_config extends RWRegister[Int](0x08) {}  // Interrupt Configuration Register
    // object irq_control extends RWRegister[Int](0x0C) {}  // Interrupt Control Register

    /** Interface Access Arbitration Control Register */
    object interface_access_arbitration_control extends RVRegister[Int](0x10) {
      val kill_other_if_req = RWBit(31)  // Kill of other interface request bit command
      // val other_if_req_stat = RWBit(21)  // Status of other interface grant bit
      // val own_if_req_stat   = RWBit(20)  // Status of own interface grant bit
      val other_if_req_stat = RWBit(17)  // Status of other interface request bit
      val own_if_req_stat   = RWBit(16)  // Status of own interface request bit
      val own_if_req        = RWBit( 0)  // Own interface request bit
    }

    // object cblt_broadcast extends RWRegister[Int](0x14) {}  // Broadcast Setup Register

    /** Hardware Version Register */
    object hardware_version extends RORegister[Int](0x1C) {
      val version           = ROBits( 0 to  3)  // Hardware Version
    }


    // ADC interface registers
    // -----------------------

    /** Temperature Register */
    object internal_temperature_reg extends RWRegister[Int](0x20) {
      val temperature       = ROBits( 0 to  9) withConv LinearConv(scale = 0.25)  // Temperature Data
    }

    // object one_wire_control_reg extends RWRegister[Int](0x24) {}  // Onewire EEPROM Control Register

    /** Serial Number Register */
    object serial_number_reg extends RORegister[Int](0x28) {
      val dhcp_option       = ROBits(24 to 31)  // DHCP Option Value
      val serno_invalid     = ROBit(16)  // Serial Number Not Valid Flag
      val serno             = ROBits(0 to 15)  // Serial Number
    }

    // object internal_transfer_speed_reg extends RWRegister[Int](0x2C) {}  // Internal Transfer Speed register
    // object adc_fpga_boot_csr extends RWRegister[Int](0x30) {}  // ADC FPGA Boot control register
    // object spi_flash_csr extends RWRegister[Int]() {0x34}  // SPI Flash Control/Status register
    // object spi_flash_data extends RWRegister[Int]() {0x38}  // SPI Flash Data register

    /** Programmable Clock I2C Register */
    class ClkOscI2CRegister(address: MemAddress) extends RVRegister[Int](address) {
      val read_byte         = RWBit(13)  // Read byte, put ACK
      val write_byte        = RWBit(12)  // Write byte, put ACK
      val stop              = RWBit(11)  // STOP
      val repeat_start      = RWBit(10)  // Repeat START
      val start             = RWBit( 9)  // START
      val ack               = RWBit( 8)  // Read: Received Ack on write cycle; Write: Ack on read cycle
      val data              = RWBits(0 to 7)  // Read: Read data; Write: Write data
    }

    /** Programmable ADC Clock I2C Register */
    val clk_osc_i2c_reg = new ClkOscI2CRegister(0x40)

    // val clk_mgt1_i2c_reg = new ClkOscI2CRegister(0x44)  // not used, according to manual
    // val clk_mgt2_i2c_reg = new ClkOscI2CRegister(0x48)  // not used, according to manual
    // val clk_dd3_i2c_reg = new ClkOscI2CRegister(0x4C)  // not used, according to manual

    /** ADC Sample Clock distribution control Register */
    object sample_clock_distribution_control extends RWRegister[Int](0x50) {
      val adc_clk_mux       = RWBits(0 to 1)  // ADC Sample Clock Multiplexer select bits
        // (0: Internal osc.; 1: VXS; 2: FP-LVDS; 3: FP-NIM)
    }

    /** External NIM Clock Multiplier SPI Register
      *
      * (Cmd Bit 1, Cmd Bit 0) == (0, 0): Execute SPI Write/Read Cmd
      * (Cmd Bit 1, Cmd Bit 0) == (0, 1): Reset Cmd
      */
    object nim_clk_multiplier_spi_reg extends RVRegister[Int](0x54) {
      val cmd_bit_1         = RWBit(31)  // Write: Cmd Bit 1; Read: Read/Write Cmd BUSY Flag
      val cmd_bit_0         = RWBit(30)  // Write: Cmd Bit 0; Read: Reset Cmd BUSY Flag
      val instruction       = RWBits(8 to 15) // Write: Instruction Byte
      val addr_data         = RWBits(0 to 7)  // Write: Address/Data; Read: Data
    }

    /** FP-Bus control Register */
    object fp_lvds_bus_control extends RWRegister[Int](0x58) {
      /** FP-Bus Sample Clock Out MUX bit
        * Selects the source of the FP-Bus Sample Clock
        *
        * Value 0: Onboard programmable oscillator ( after power up: 125MHz)
        * Value 1: External clock from NIM connector CI (via programmable clock multiplier)
        */
      val smpl_clk_out_mux  = RWBit(5)

      /** FP-Bus Sample Clock Out Enable
        * Enables the Sample Clock to the FP- Bus (only on one SIS3316)
        */
      val smpl_clk_out_en   = RWBit(4)

      /** FP-Bus Status Lines Output Enable
        * Enables the Status Lines to the FP-Bus (on all SIS3316)
        */
      val stat_lines_out_en = RWBit(1)

      /** FP-Bus Control Lines Output Enable
        * Enables the Control Lines to the FP-Bus (only on one SIS3316)
        */
      val ctrl_lines_out_en = RWBit(0)
    }

    /** NIM Input Control/Status Register */
    object nim_input_control_reg extends RWRegister[Int](0x5C) {
      val input_sig_ui_stat = ROBit(25)  // Write: reserved; Read: Status of NIM Input signal UI
      val ext_in_ui_stat    = ROBit(24)  // Write: reserved; Read: Status of External NIM Input UI
      val input_sig_ti_stat = ROBit(21)  // Write: reserved; Read: Status of NIM Input signal UI
      val ext_in_ti_stat    = ROBit(20)  // Write: reserved; Read: Status of External NIM Input UI
      val in_ui_pss_en      = RWBit(13)  // NIM Input UI as PPS Enable
      val in_ui_veto_en     = RWBit(12)  // NIM Input UI as Veto Enable
      val in_ui_func        = RWBit(11)  // Set NIM Input UI Function
      val in_ui_level_sens  = RWBit(10)  // NIM Input UI Level sensitive
      val in_ui_inv         = RWBit( 9)  // NIM Input UI Invert
      val in_ui_tsclear_en  = RWBit( 8)  // NIM Input UI as Timestamp Clear Enable
      val in_ti_func        = RWBit( 7)  // Set NIM Input TI Function
      val in_ti_level_sens  = RWBit( 6)  // NIM Input TI Level sensitive
      val in_ti_inv         = RWBit( 5)  // NIM Input TI Invert
      val in_ti_trig_en     = RWBit( 4)  // NIM Input TI as Trigger Enable
      val in_ci_func        = RWBit( 3)  // Set NIM Input CI Function
      val in_ci_level_sens  = RWBit( 2)  // NIM Input CI Level sensitive
      val in_ci_inv         = RWBit( 1)  // NIM Input CI Invert
      val in_ci_en          = RWBit( 0)  // NIM Input CI Enable
    }

    /** Acquisition control/status Register (0x60, read/write)
      *
      * single_bank_mode value: 0 = double bank mode, 1 = single bank mode
      * nim_ui_bank_swap value: 0/1 = disable/enable toggling of the active Sample Bank with a signal on NIM Input UI
      * nim_ti_bank_swap value: 0/1 = disable/enable toggling of the active Sample Bank with a signal on NIM Input TI
      * nim_bank_swap_en value: 0/1 = "Sample Bank Swap Control with NIM Input TI/UI" Logic is disabled/enabled
      */
    object acquisition_control_status extends RWRegister[Int](0x60) {
      class FPGABits(offset: Int) extends SubRegister {
        val mem_addr_thresh = ROBit(offset + 1)  // Write: reserved; Read: Status of Memory Address Threshold Flag
        val smpl_busy       = ROBit(offset + 0)  // Write: reserved; Read: Status of Sample Logic Busy
      }

      val fpga4 = new FPGABits(30)  // FPGA 4 bits
      val fpga3 = new FPGABits(28)  // FPGA 3 bits
      val fpga2 = new FPGABits(26)  // FPGA 2 bits
      val fpga1 = new FPGABits(24)  // FPGA 1 bits

      val fpga = Map(1 -> fpga1, 2 -> fpga2, 3 -> fpga3, 4 -> fpga4)

      val pps_latch_stat    = ROBit(23)  // Write: reserved; Read: Status of "PPS latch bit"
      val nim_bank_swap_en  = ROBit(22)  // Write: reserved; Read: Status of "Sample Bank Swap Control with NIM Input TI/UI" Logic enabled
      val fp_addr_thresh    = ROBit(21)  // Write: reserved; Read: Status of FP-Bus-In Status 2: Address Threshold flag
      val fp_smpl_busy      = ROBit(20)  // Write: reserved; Read: Status of FP-Bus-In Status 1: Sample Logic busy
      val mem_addr_thresh   = ROBit(19)  // Write: reserved; Read: Status of Memory Address Threshold flag (OR)
      val smpl_busy         = ROBit(18)  // Write: reserved; Read: Status of Sample Logic Busy (OR)
      val smpl_armed_bank   = ROBit(17) withConv ((1, 2))  // Write: reserved; Read: Status of ADC Sample Logic Armed On Bank2 flag
      val smpl_armed        = ROBit(16)  // Write: reserved; Read: Status of ADC Sample Logic Armed
      val ext_trig_dis_on_b = RWBit(15)  // External Trigger Disable with internal Busy select
      val int_trig_to_ext   = RWBit(14)  // Feedback Selected Internal Trigger as External Trigger Enable
      val nim_ui_bank_swap  = RWBit(13)  // NIM Input UI as "disarm Bankx and arm alternate Bank" command Enable
      val nim_ti_bank_swap  = RWBit(12)  // NIM Input TI as "disarm Bankx and arm alternate Bank" command Enable
      val local_veto        = RWBit(11)  // Local Veto function as Veto Enable
      val ext_tsclear_en    = RWBit(10)  // External Timestamp-Clear function Enable
      val ext_trig_as_veto  = RWBit( 9)  // External Trigger function as Veto Enable
      val ext_trig_en       = RWBit( 8)  // External Trigger function as Trigger Enable
      val fp_smpl_ctrl_en   = RWBit( 7)  // FP-Bus-In Sample Control Enable
      val fp_ctrl2_en       = RWBit( 6)  // FP-Bus-In Control 2 Enable
      val fp_ctrl2_as_veto  = RWBit( 5)  // FP-Bus-In Control 1 as Veto Enable
      val fp_ctrl1_as_trig  = RWBit( 4)  // FP-Bus-In Control 1 as Trigger Enable
      val single_bank_mode  = RWBit( 0)  // Single Bank Mode Enable (reserved)
    }

    /** Trigger Coincidence Lookup Table Control Register */
    object lookup_table_control_reg extends RVRegister[Int](0x64) {
      val table_clear       = RWBit(31)  // Write: Lookup Table Clear command; Read: Status Clear Busy
      val table2_pulse_len  = RWBits(8 to 15)  // Lookup Table 2 Coincidence output pulse length
      val table1_pulse_len  = RWBits(0 to 7)   // Lookup Table 1 Coincidence output pulse length
    }

    /** Trigger Coincidence Lookup Table Address Register */
    object lookup_table_addr_reg extends RVRegister[Int](0x68) {
      val mask              = RWBits(16 to 31)  // Lookup Table 1 and 2  Channel Trigger Mask
      val rw_addr           = RWBits(0 to 15)  // Lookup Table 1 and 2 Write/Read Address
    }

    /** Trigger Coincidence Lookup Table Data register */
    object lookup_table_data_reg extends RVRegister[Int](0x6C) {
      val table2_val        = RWBit(1)  // Lookup Table 2 Coincidence validation bit
      val table1_val        = RWBit(0)  // Lookup Table 1 Coincidence validation bit
    }

    /** LEMO Out "CO" Select register */
    object lemo_out_co_select_reg extends RWRegister[Int](0x70) {}  // TODO: Add content

    /** LEMO Out "TO" Select register */
    object lemo_out_to_select_reg extends RWRegister[Int](0x74) {}  // TODO: Add content

    /** LEMO Out "UO" Select register */
    object lemo_out_uo_select_reg extends RWRegister[Int](0x78) {}  // TODO: Add content

    /** Internal Trigger Feedback Select register */
    object internal_trigger_feedback_select_reg extends RWRegister[Int](0x7C) {
      val lut1_coind_sel    = RWBit(24)  // Select Lookup Table 1 Coincidence stretched output pulse
      val sel_sum_trig_grp4 = RWBit(19)  // Select internal SUM-Trigger stretched pulse ch13-16
      val sel_sum_trig_grp3 = RWBit(18)  // Select internal SUM-Trigger stretched pulse ch19-12
      val sel_sum_trig_grp2 = RWBit(17)  // Select internal SUM-Trigger stretched pulse ch5-8
      val sel_sum_trig_grp1 = RWBit(16)  // Select internal SUM-Trigger stretched pulse ch1-4
      val int_trig_sel_mask = RWBits(0 to 15)  // Select internal Trigger stretched pulse ch 1 to 16

      val sel_sum_trig = Map(
        1 -> sel_sum_trig_grp1, 2 -> sel_sum_trig_grp2,
        3 -> sel_sum_trig_grp3, 4 -> sel_sum_trig_grp4
      )
    }


    /** ADC FPGA Data Transfer Control Register */
    class DataTransferCtlReg(address: MemAddress) extends RVRegister[Int](address) {
      import DataTransferCtlReg.{Cmd, MemSpaceSel}

      val cmd               = RWBits(30 to 31) withConv Cmd  // Cmd
      val mem_space_sel     = RWBits(28 to 29) withConv MemSpaceSel  // Space select bits
      val mem_addr          = RWBits(0 to 27)  // Memory 32-bit start address (128 Meg x 16 : only address bits 25-0 are used )

      def tiedValue(cmd: Cmd.Value, mem_space_sel: MemSpaceSel.Value = MemSpaceSel.Mem1, mem_addr: Int = 0) =
        zero +| (this.cmd, cmd) +| (this.mem_space_sel, mem_space_sel) +| (this.mem_addr, mem_addr)
    }

    object DataTransferCtlReg {
      object Cmd extends Enumeration {
        val Reset           = Value(0)  // Reset Transfer FSM
        val Reset_Also      = Value(1)  // 
        val Read            = Value(2)  // Start Read Transfer
        val Write           = Value(3)  // Start Write Transfer
      }

      object MemSpaceSel extends Enumeration {
        val Mem1            = Value(0)  // Memory 1 (Ch. 1 and 2, resp. 5 and 6, etc.)
        val Mem2            = Value(1)  // Memory 2 (Ch. 3 and 4, resp. 7 and 8, etc.)
        val StatCounter     = Value(3)  // Statistic Counter (128 32-bit words)
      }
    }

    /** ADC FPGA Data Transfer Control Register, Ch. 1 to 4 */
    val data_transfer_ch1_4_ctrl_reg = new DataTransferCtlReg(0x80)

    /** ADC FPGA Data Transfer Control Register, Ch. 5 to 8 */
    val data_transfer_ch5_8_ctrl_reg = new DataTransferCtlReg(0x84)

    /** ADC FPGA Data Transfer Control Register, Ch. 9 to 12 */
    val data_transfer_ch9_12_ctrl_reg = new DataTransferCtlReg(0x88)

    /** ADC FPGA Data Transfer Control Register, Ch. 13 to 16 */
    val data_transfer_ch13_16_ctrl_reg = new DataTransferCtlReg(0x8C)

    /** ADC FPGA Data Transfer Control Registers */
    val data_transfer_ctrl_reg = Map(
      1 -> data_transfer_ch1_4_ctrl_reg, 2 -> data_transfer_ch5_8_ctrl_reg,
      3 -> data_transfer_ch9_12_ctrl_reg, 4 -> data_transfer_ch13_16_ctrl_reg
    )


    /** ADC FPGA Data Transfer Status Register */
    class DataTransferStatusReg(address: MemAddress) extends RORegister[Int](address) {
      val busy              = ROBit(31)  // Data Transfer Logic busy
      val direction         = ROBit(30)  // Data Transfer Direction (Write-Flag; 0: Memory -> VME FPGA; 1: VME FPGA -> Memory)
      val fifo_near_full    = ROBit(28)  // FIFO (read VME FIFO) Data AlmostFull Flag
      val max_no_pend_read  = ROBit(27)  // "max_nof_pending_read_requests"
      val no_pend_read      = ROBit(26)  // "no_pending_read_requests"
      val int_addr_counter  = ROBits(0 to 25)  // Data Transfer internal 32-bit Address counter
    }

    /** ADC FPGA Data Transfer Status Register, Ch. 1 to 4 */
    val data_transfer_adc1_4_status_reg = new DataTransferStatusReg(0x90) {}

    /** ADC FPGA Data Transfer Status Register, Ch. 5 to 8 */
    val data_transfer_adc5_8_status_reg = new DataTransferStatusReg(0x94) {}

    /** ADC FPGA Data Transfer Status Register, Ch. 9 to 12 */
    val data_transfer_adc9_12_status_reg = new DataTransferStatusReg(0x98) {}

    /** ADC FPGA Data Transfer Status Register, Ch. 13 to 16 */
    val data_transfer_adc13_16_status_reg = new DataTransferStatusReg(0x9C) {}

    /** ADC FPGA Data Transfer Status Registers */
    val data_transfer_status_reg = Map(
      1 -> data_transfer_adc1_4_status_reg, 2 -> data_transfer_adc5_8_status_reg,
      3 -> data_transfer_adc9_12_status_reg, 4 -> data_transfer_adc13_16_status_reg
    )


    /** VME FPGA – ADC FPGA Data Link Status register */
    object vme_fpga_link_adc_prot_status extends RVRegister[Int](0xA0) {
      class FPGABits(offset: Int) extends SubRegister {
        val frame_err_cl      = RWBit(offset + 7)  // Write: ADC FPGA: Clear Frame_error_latch; Read: ADC FPGA: Frame_error_latch
        val soft_err_cl       = RWBit(offset + 6)  // Write: ADC FPGA: Clear Soft_error_latch; Read: ADC FPGA: Soft_error_latch
        val hard_err_cl       = RWBit(offset + 5)  // Write: ADC FPGA: Clear Hard_error_latch; Read: ADC FPGA: Hard_error_latch
        val lane_up           = ROBit(offset + 4)  // Read: ADC FPGA: Lane_up_flag
        val ch_up             = ROBit(offset + 3)  // Read: ADC FPGA: Channel_up_flag
        val frame_err         = ROBit(offset + 2)  // Read: ADC FPGA: Frame_error_flag
        val soft_err          = ROBit(offset + 1)  // Read: ADC FPGA: Soft_error_flag
        val hard_err          = ROBit(offset + 0)  // Read: ADC FPGA: Hard_error_flag
      }

      val fpga4 = new FPGABits(24)  // FPGA 4 bits
      val fpga3 = new FPGABits(16)  // FPGA 3 bits
      val fpga2 = new FPGABits( 8)  // FPGA 2 bits
      val fpga1 = new FPGABits( 0)  // FPGA 1 bits

      val fpga = Map(1 -> fpga1, 2 -> fpga2, 3 -> fpga3, 4 -> fpga4)
    }

    /* ADC FPGA SPI BUSY Status register */
    object fpga_spi_busy_status_reg extends RORegister[Int](0xA4) {
      val busy          = ROBit(31)  // ADC FPGAx: Busy flag (or of ADC FPGA 1 to 4 Busy flags)
      val fpga4_busy    = ROBit( 3)  // ADC FPGA 4: Busy flag
      val fpga3_busy    = ROBit( 2)  // ADC FPGA 3: Busy flag
      val fpga2_busy    = ROBit( 1)  // ADC FPGA 2: Busy flag
      val fpga1_busy    = ROBit( 0)  // ADC FPGA 1: Busy flag
    }


    /** Prescaler Output Pulse Divider register */
    val prescaler_output_pulse_divider_reg = new RVRegister[Int](0xB8)

    /** Prescaler Output Pulse Length register */
    val prescaler_output_pulse_length_reg = new RVRegister[Int](0xBC)


    // ADC key registers

    val key_reset = new KeyRegister[Int](0x400)
    val key_user_function = new KeyRegister[Int](0x404)

    // val key_arm = new KeyRegister[Int](0x410)  // Key address: Arm sample logic

    /** Key address: Disarm sample logic */
    val key_disarm = new KeyRegister[Int](0x414)

    /** Key address: Trigger */
    val key_trigger = new KeyRegister[Int](0x418)

    /** Key address: Timestamp Clear */
    val key_timestamp_clear = new KeyRegister[Int](0x41C)


    /** Key address: Disarm Bankx and Arm Bank1 */
    val key_disarm_and_arm_bank1 = new KeyRegister[Int](0x420)

    /** Key address: Disarm Bankx and Arm Bank2 */
    val key_disarm_and_arm_bank2 = new KeyRegister[Int](0x424)

    /** Key address: Disarm Bankx and Arm Banky */
    val key_disarm_and_arm_bank = Map(1 -> key_disarm_and_arm_bank1, 2 -> key_disarm_and_arm_bank2)


    /** Key address: Enable Sample Bank Swap Control with NIM Input TI/UI Logic */
    val key_enable_sample_bank_swap_control_with_nim_input = new KeyRegister[Int](0x428)

    /** Key address: Disable Prescaler Output Pulse Divider logic */
    val key_disable_prescaler_output_pulse_divider = new KeyRegister[Int](0x42C)

    /** Key address: PPS_Latch_Bit_clear */
    val key_pps_latch_bit_clear = new KeyRegister[Int](0x430)

    /** Key address: Reset ADC-FPGA-Logic */
    val key_adc_fpga_reset = new KeyRegister[Int](0x434)

    /** Key address: ADC Clock DCM/PLL Reset */
    val key_adc_clock_dcm_reset = new KeyRegister[Int](0x438)


    /** ADC FPGA Registers */
    class FPGARegsRegion(from: MemAddress) extends SubRegion(from, from + 0x001000) {

      /** ADC Input Tap Delay Register */
      object input_tap_delay_reg extends RVRegister[Int](0x0000) {
        val half_smpl_delay   = RWBit(12)  // Add 1⁄2 Sample Clock period delay bit (since ADC Version V-0250-0004 and V-0125-0004)
        val calib             = RWBit(11)  // Calibration
        val lnk_err_latch_clr = RWBit(10)  // Clear Link Error Latch bits
        val sel_ch_34         = RWBit(9)  // Ch. 3 and 4 select
        val sel_ch_12         = RWBit(8)  // Ch. 1 and 2 select
        val tap_delay         = RWBits(0 to 7)  // Tap delay value (times 40ps, max. 1⁄2 Sample Clock period)
      }

      /** ADC Gain and Termination Control Register
        *
        * Gain control value:
        *     * 0: 5 V
        *     * 1: 2 V
        *     * 2: 1.9 V
        *     * 3: 1.9 V
        */
      object analog_ctrl_reg extends RWRegister[Int](0x0004) {
        class ChBits(offset: Int) extends SubRegister {
          val term_dis        = RWBit(offset + 2)  // Disable 50 Ohm Termination
          val gain_ctrl       = RWBits(offset + 0 to offset + 1)  // Gain Control
        }

        val ch4 = new ChBits(24)  // Ch. 4 bits
        val ch3 = new ChBits(16)  // Ch. 3 bits
        val ch2 = new ChBits( 8)  // Ch. 2 bits
        val ch1 = new ChBits( 0)  // Ch. 1 bits

        val ch = Map(1 -> ch1, 2 -> ch2, 3 -> ch3, 4 -> ch4)
      }

      /** ADC Offset (DAC) Control Register */
      object dac_offset_ctrl_reg extends WORegister[Int](0x0008) {
        val crtl_mode         = WOBits(29 to 31)  // DAC Ctrl Mode
        val command           = WOBits(24 to 27)  // DAC Command
        val dac_addr          = WOBits(20 to 23)  // DAC Address
        val data              = WOBits(4 to 19)  //  DAC Data
      }

      /** ADC Offset (DAC) Readback Register */
      object dac_offset_readback_reg extends RORegister[Int](0x0108) {
        val data              = ROBits(0 to 15)  // DAC Read Data
      }

      /** ADC SPI Control Register */
      object spi_ctrl_reg extends RVRegister[Int](0x000C) {
        val cmd               = RWBits(30 to 31) withConv Cmd  // Command
        val data_out_en       = RWBit(24)  // ADC Data Output Enable
        val sel_ch34          = RWBit(22)  // Select ADCx ch3/ch4 bit
        val rw_addr           = RWBits(8 to 20)  // Address
        val data              = RWBits(0 to 7)  // Write Data

        object Cmd extends Enumeration {
          val NoOp            = Value(0)  // no function
          val Reserved        = Value(1)  // Reserved (ADC Synch Cmd)
          val Write           = Value(2)  // Write Cmd (relevant bits: 22 and 20 to 0)
          val Read            = Value(3)  // Read Cmd (relevant bits: 22 and 20 to 0)
        }
      }


      /** ADC SPI Readback Register */
      object spi_readback_reg extends RORegister[Int](0x010C) {
        val data              = ROBits(0 to 15)  // Read Data
      }

      /** Event Configuration Register */
      object event_config_reg extends RWRegister[Int](0x0010) {
        class ChBits(offset: Int) extends SubRegister {
          val ext_veto_en       = RWBit(offset + 7)  // External Veto Enable bit
          val ext_gate_en       = RWBit(offset + 6)  // External Gate Enable bit
          val int_gate2_en      = RWBit(offset + 5)  // Internal Gate 2 Enable bit
          val int_gate1_en      = RWBit(offset + 4)  // Internal Gate 1 Enable bit
          val ext_trig_en       = RWBit(offset + 3)  // External Trigger Enable bit
          val int_trig_en       = RWBit(offset + 2)  // Internal Trigger Enable bit
          val sum_trig_en       = RWBit(offset + 1)  // Internal SUM-Trigger Enable bit
          val input_inv         = RWBit(offset + 0)  // Input Invert bit
        }

        val ch4 = new ChBits(24)  // Ch. 4 bits
        val ch3 = new ChBits(16)  // Ch. 3 bits
        val ch2 = new ChBits( 8)  // Ch. 2 bits
        val ch1 = new ChBits( 0)  // Ch. 1 bits

        val ch = Map(1 -> ch1, 2 -> ch2, 3 -> ch3, 4 -> ch4)
      }

      /** Channel Header ID Register */
      object channel_header_reg extends RWRegister[Int](0x0014) {
        val id                = RWBits(20 to 31)  // Channel Header/ID
      }

      /** End Address Threshold Register */
      object address_threshold_reg extends RWRegister[Int](0x0018) {
        val stop_on_thresh    = RWBit(31)  // "Suppress saving of more Hits/Events if Memory Address Threshold Flag is valid" Enable
        val addr_thresh_value = RWBits(0 to 23) withConv (ValueConv(1 to 0xff0000) andThen IntMultOffsetConv(scale = 4)) // End Address Threshold value
      }

      /** Active Trigger Gate Window Length Register */
      object trigger_gate_window_length_reg extends RWRegister[Int](0x001C) {
        val length            = RWBits(0 to 15)  // Active Trigger Gate Window Length (bit 0 not used)
      }

      /** Raw Data Buffer Configuration Register
        *
        * Note: Bit 0 of start_index and sample_length is always zero (since
        * data is stored in packets of 2 consecutive samples).
        */
      object raw_data_buffer_config_reg extends RWRegister[Int](0x0020) {
        val sample_length     = RWBits(16 to 31)  // Raw Buffer Sample_Length
        val start_index       = RWBits(0 to 15)  // Raw Buffer_Start_Index
      }

      /** Pileup Configuration Register
        *
        * Note: Bit 0 of repileup_win_len and pileup_win_len is always zero.
        */
      object pileup_config_reg extends RWRegister[Int](0x0024) {
        val repileup_win_len  = RWBits(16 to 31)  // Re-Pileup Window Length
        val pileup_win_len    = RWBits(0 to 15)  // Pileup Window Length
      }

      /** Pre-Trigger Delay Register
        *
        * Note: Bit 0 of delay is always zero. Maximum value of delay is 2042
        * for ADC FPGA firmware versions <= 0006 and 16378 for versions
        * >= 0007.
        */
      object pre_trigger_delay_reg extends RWRegister[Int](0x0028) {
        val additional        = RWBit(15)  // Additional Delay of Fir Trigger P+G Bit
        val delay             = RWBits(0 to 13)  // Pretrigger Delay
      }

      /** Average Configuration Register
        *
        * Only present for SIS3316-16bit.
        *
        * Note: Valid values for pretrig_delay are 0, 1, 2 to 4094. Valid
        * values for sample_len are 0, 2, 4, 6 to 65534 (bit 0 of sample_len)
        * is always zero).
        */
      object average_configuration_reg extends RWRegister[Int](0x002C) {
        val mode              = RWBits(28 to 30)  // Average Mode
        val pretrig_delay     = RWBits(16 to 27)  // Average Pretrigger Delay
        val sample_len        = RWBits(0 to 15)  // Average Sample Length
      }

      /** Data Format Configuration Register
        *
        * sel_test_buf value: 0 for FIR Trigger MAW selected, 1 for FIR Energy MAW selected
        */
      object dataformat_config_reg extends RWRegister[Int](0x0030) {
        class ChBits(offset: Int) extends SubRegister {
          val sel_test_buf      = RWBit(offset + 5)  // Select Energy MAW Test Buffer
          val save_maw_test     = RWBit(offset + 4)  // Save MAW Test Buffer Enable
          val save_energy       = RWBit(offset + 3)  // Save Start Energy MAW value and Max. Energy MAW value
          val save_ft_maw       = RWBit(offset + 2)  // Save 3 x Fast Trigger MAW values (max value, value before Trigger, value with Trigger)
          val save_acc_78       = RWBit(offset + 1)  // Save 2 x Accumulator values (Gates 7,8)
          val save_ph_acc16     = RWBit(offset + 0)  // Save Peak High values and 6 x Accumulator values (Gates 1,2, ..,6)
        }

        val ch4 = new ChBits(24)  // Ch. 4 bits
        val ch3 = new ChBits(16)  // Ch. 3 bits
        val ch2 = new ChBits( 8)  // Ch. 2 bits
        val ch1 = new ChBits( 0)  // Ch. 1 bits

        val ch = Map(1 -> ch1, 2 -> ch2, 3 -> ch3, 4 -> ch4)
      }

      /** MAW Test Buffer Configuration Register
        *
        * Note:
        * Valid values for pretrig_delay: 2, 4, 6 to 1022.
        * Valid values for buffer_len: 0, 2, 4, 6 to 1022.
        * Bit 0 of pretrig_delay and buffer_len is always zero.
        */
      object maw_test_buffer_config_reg extends RWRegister[Int](0x0034) {
        val pretrig_delay  = RWBits(16 to 25)  // MAW Test Buffer Pretrigger Delay
        val buffer_len  = RWBits(0 to 10)  // MAW Test Buffer Length
      }

      /** Internal Trigger Delay Configuration Register
        *
        * Delay time is delay value multiplied by 2 clock cycles.
        */
      object internal_trigger_delay_config_reg extends RWRegister[Int](0x0038) {
        class ChBits(offset: Int) extends SubRegister {
          val delay           = RWBits(offset + 0 to offset + 7)  // Internal Trigger Delay
        }

        val ch4 = new ChBits(24)  // Ch. 4 bits
        val ch3 = new ChBits(16)  // Ch. 3 bits
        val ch2 = new ChBits( 8)  // Ch. 2 bits
        val ch1 = new ChBits( 0)  // Ch. 1 bits

        val ch = Map(1 -> ch1, 2 -> ch2, 3 -> ch3, 4 -> ch4)
      }

      /** Internal Gate Length Configuration Register
        *
        * Gate time is length value multiplied by 2 clock cycles.
        */
      object internal_gate_length_config_reg extends RWRegister[Int](0x003C) {
        class ChBits(offset: Int) extends SubRegister {
          val gate2_en          = RWBit(offset + 4)  // Gate 2 Enable
          val gate1_en          = RWBit(offset + 0)  // Gate 1 Enable
        }

        val ch4 = new ChBits(19)  // Ch. 4 bits
        val ch3 = new ChBits(18)  // Ch. 3 bits
        val ch2 = new ChBits(17)  // Ch. 2 bits
        val ch1 = new ChBits(16)  // Ch. 1 bits

        val ch = Map(1 -> ch1, 2 -> ch2, 3 -> ch3, 4 -> ch4)

        val gate_len        = RWBits(8 to 15)  // Internal Gate Length
        val coind_gate_len  = RWBits(0 to 7)  // Internal Coincidence Gate Length
      }


      /** FIR Trigger Setup Register
        * Note:
        * Valid values for gap_time and peak_time: 2, 4, 6, ...., 510 (bit 0
        * of g and p is not used).
        * Valid values for nim_tp_len: 2, 4, 6, ...., 256.
        */
      class FIRTriggerSetupRegister(address: MemAddress) extends RWRegister[Int](address) {
        val nim_tp_len        = RWBits(24 to 31)  // External NIM Out Trigger Pulse Length (streched).
        val gap_time          = RWBits(12 to 23)  // G: Gap time (Flat Time)
        val peak_time         = RWBits(24 to 31)  // P : Peaking time
      }

      /** Ch. 1 FIR Trigger Setup Register */
      val ch1_fir_trigger_setup_reg = new FIRTriggerSetupRegister(0x0040)

      /** Ch. 2 FIR Trigger Setup Register */
      val ch2_fir_trigger_setup_reg = new FIRTriggerSetupRegister(0x0050)

      /** Ch. 3 FIR Trigger Setup Register */
      val ch3_fir_trigger_setup_reg = new FIRTriggerSetupRegister(0x0060)

      /** Ch. 4 FIR Trigger Setup Register */
      val ch4_fir_trigger_setup_reg = new FIRTriggerSetupRegister(0x0070)

      /** FIR Trigger Setup Registers */
      val fir_trigger_setup_reg = Map(
        1 -> ch1_fir_trigger_setup_reg, 2 -> ch2_fir_trigger_setup_reg,
        3 -> ch3_fir_trigger_setup_reg, 4 -> ch4_fir_trigger_setup_reg
      )

      /** Sum FIR Trigger Setup register */
      val sum_fir_trigger_setup_reg = new FIRTriggerSetupRegister(0x0080)


      /** Trigger Threshold Register
        *
        * Note: High energy suppression only works with CFD enabled.
        */
      class TriggerThresholdRegister(address: MemAddress) extends RWRegister[Int](address) {
        val trig_en           = RWBit(31)  // Trigger enable
        val high_e_suppr      = RWBit(30)  // High Energy Suppress Trigger Mode
        val cfd_ctrl          = RWBits(28 to 29) withConv CfdCtrl  // CFD control bits
        val threshold         = RWBits(0 to 27) withConv IntMultOffsetConv(offset = -0x8000000)  // Trigger threshold value
      }

      /** Ch. 1 Trigger Threshold Register */
      val ch1_fir_trigger_threshold_reg = new TriggerThresholdRegister(0x0044)

      /** Ch. 2 Trigger Threshold Register */
      val ch2_fir_trigger_threshold_reg = new TriggerThresholdRegister(0x0054)

      /** Ch. 3 Trigger Threshold Register */
      val ch3_fir_trigger_threshold_reg = new TriggerThresholdRegister(0x0064)

      /** Ch. 4 Trigger Threshold Register */
      val ch4_fir_trigger_threshold_reg = new TriggerThresholdRegister(0x0074)

      /** Trigger Threshold Registers */
      val fir_trigger_threshold_reg = Map(
        1 -> ch1_fir_trigger_threshold_reg, 2 -> ch2_fir_trigger_threshold_reg,
        3 -> ch3_fir_trigger_threshold_reg, 4 -> ch4_fir_trigger_threshold_reg
      )

      /** Sum Trigger Threshold Register */
      val sum_fir_trigger_threshold_reg = new FIRTriggerSetupRegister(0x0084)


      /** High Energy Trigger Threshold Register */
      class HighETriggerThresholdRegister(address: MemAddress) extends RWRegister[Int](address) {
        val trig_both_edges   = RWBit(31)  // Trigger on both edges enable bit
        val trig_out          = RWBit(28)  // High Energy stretched Trigger Out select bit
        val threshold         = RWBits(0 to 27)  // High Energy Trigger Threshold value
      }

      /** Ch. 1 High Energy Trigger Threshold Register */
      val ch1_fir_high_energy_threshold_reg = new HighETriggerThresholdRegister(0x0048)

      /** Ch. 2 High Energy Trigger Threshold Register */
      val ch2_fir_high_energy_threshold_reg = new HighETriggerThresholdRegister(0x0058)

      /** Ch. 3 High Energy Trigger Threshold Register */
      val ch3_fir_high_energy_threshold_reg = new HighETriggerThresholdRegister(0x0068)

      /** Ch. 4 High Energy Trigger Threshold Register */
      val ch4_fir_high_energy_threshold_reg = new HighETriggerThresholdRegister(0x0078)

      /** High Energy Trigger Threshold Registers */
      val fir_high_energy_threshold_reg = Map(
        1 -> ch1_fir_high_energy_threshold_reg, 2 -> ch2_fir_high_energy_threshold_reg,
        3 -> ch3_fir_high_energy_threshold_reg, 4 -> ch4_fir_high_energy_threshold_reg
      )

      /** Sum High Energy Trigger Threshold Register */
      val sum_fir_high_energy_threshold_reg = new HighETriggerThresholdRegister(0x0088)


      /** Trigger Statistic Counter Mode Register
        *
        * update_mode value:
        *     * 0: Readout of actual Trigger-Statistic-Counters
        *     * 1: Readout of the latched Trigger-Statistic-Counters (latch
        *          on bank switch)
        */
      object trigger_statistic_counter_mode_reg extends RWRegister[Int](0x0090) {
        val update_mode       = RWBit(0)  // Update Mode
      }

      /** Peak/Charge Configuration Register
        *
        * Valid values for bl_pregate_delay: 0, 2, 4, 6 to 510 (bit 0 is always zero).
        */
      object peak_charge_configuration_reg extends RWRegister[Int](0x0094) {
        val peak_charge_en    = RWBit(31)  // Enable Peak/Charge Mode
        val bl_avg_mode       = RWBits(28 to 29)  // Baseline Average Mode
        val bl_pregate_delay  = RWBits(16 to 27)  // Baseline Pregate Delay
      }

      /** Extended Raw Data Buffer Configuration Register
        *
        * Maximum value of sample_len is (0x2000000 - 2), bit 0 is always
        * zero.
        */
      object extended_raw_data_buffer_config_reg extends RWRegister[Int](0x0098) {
        val sample_len        = RWBits(0 to 24)  // Extended Raw Buffer Sample Length
      }


      /** Accumulator Gate Configuration Register */
      class AccGateConfigRegister(address: MemAddress) extends RWRegister[Int](address) {
        val gate_len          = RWBits(16 to 24)  // Gate Length
        val gate_start        = RWBits(0 to 15)  // Gate Start Index (Address)
      }

      /** Accumulator Gate 1 Configuration Register */
      val accumulator_gate1_config_reg = new AccGateConfigRegister(0x00A0)

      /** Accumulator Gate 2 Configuration Register */
      val accumulator_gate2_config_reg = new AccGateConfigRegister(0x00A4)

      /** Accumulator Gate 3 Configuration Register */
      val accumulator_gate3_config_reg = new AccGateConfigRegister(0x00A8)

      /** Accumulator Gate 4 Configuration Register */
      val accumulator_gate4_config_reg = new AccGateConfigRegister(0x00AC)

      /** Accumulator Gate 5 Configuration Register */
      val accumulator_gate5_config_reg = new AccGateConfigRegister(0x00B0)

      /** Accumulator Gate 6 Configuration Register */
      val accumulator_gate6_config_reg = new AccGateConfigRegister(0x00B4)

      /** Accumulator Gate 7 Configuration Register */
      val accumulator_gate7_config_reg = new AccGateConfigRegister(0x00B8)

      /** Accumulator Gate 8 Configuration Register */
      val accumulator_gate8_config_reg = new AccGateConfigRegister(0x00BC)

      /** Accumulator Gate Configuration Registers */
      val accumulator_config_reg = Map(
        1 -> accumulator_gate1_config_reg, 2 -> accumulator_gate2_config_reg,
        3 -> accumulator_gate3_config_reg, 4 -> accumulator_gate4_config_reg,
        5 -> accumulator_gate5_config_reg, 6 -> accumulator_gate6_config_reg,
        7 -> accumulator_gate7_config_reg, 8 -> accumulator_gate8_config_reg
      )


      /** FIR Energy Setup Register
        *
        * Valid values:
        *     * tau_factor: 0, 1, 2, ...., 63
        *     * gap_time: 2, 4, 6, ...., 510 (bit 0 is not used)
        *     * peak_time: 2, 4, 6, ...., 2044 (bit 0 is not used)
        *
        * Extra filter value:
        *     * 0: No extra filter
        *     * 1: Average of 4
        *     * 2: Average of 8
        *     * 3: Average of 16
        */
      class FIREnergySetupRegister(address: MemAddress) extends RWRegister[Int](address) {
        val tau_table         = RWBits(30 to 31)  // Tau table selection
        val tau_factor        = RWBits(24 to 29)  // Tau factor
        val extra_filter      = RWBits(22 to 23)  // Extra filter
        val gap_time          = RWBits(12 to 21)  // G: Gap time (Flat Time)
        val peak_time         = RWBits( 0 to 11)  // P : Peaking time
      }

      /** Ch. 1 FIR Energy Setup Register */
      val ch1_fir_energy_setup_reg = new FIREnergySetupRegister(0x00C0)

      /** Ch. 2 FIR Energy Setup Register */
      val ch2_fir_energy_setup_reg = new FIREnergySetupRegister(0x00C4)

      /** Ch. 3 FIR Energy Setup Register */
      val ch3_fir_energy_setup_reg = new FIREnergySetupRegister(0x00C8)

      /** Ch. 4 FIR Energy Setup Register */
      val ch4_fir_energy_setup_reg = new FIREnergySetupRegister(0x00CC)

      /** FIR Energy Setup Registers */
      val fir_energy_setup_reg = Map(
        1 -> ch1_fir_energy_setup_reg, 2 -> ch2_fir_energy_setup_reg,
        3 -> ch3_fir_energy_setup_reg, 4 -> ch4_fir_energy_setup_reg
      )


      /** Energy Histogram Configuration Register */
      class EHistConfigRegister(address: MemAddress) extends RWRegister[Int](address){
        val mem_evt_write_dis = RWBit(31)  // Writing Hits/Events into Event Memory Disable bit
        val clear_hist_w_ts   = RWBit(30)  // Histogram clear with Timestamp-Clear Disable bit
        val energy_div        = RWBits(16 to 27)  // Energy Divider (value = 0 is not allowed)
        val energy_offs       = RWBits(8 to 11)  // Energy Subtract Offset
        val pileup_en         = RWBit(1)  // Pileup Enable bit
        val hist_en           = RWBit(0)  // Histogramming Enable bit
      }

      /** Ch. 1 Energy Histogram Configuration Register */
      val ch1_histogram_conf_reg = new EHistConfigRegister(0x00D0)

      /** Ch. 1 Energy Histogram Configuration Register */
      val ch2_histogram_conf_reg = new EHistConfigRegister(0x00D4)

      /** Ch. 1 Energy Histogram Configuration Register */
      val ch3_histogram_conf_reg = new EHistConfigRegister(0x00D8)

      /** Ch. 1 Energy Histogram Configuration Register */
      val ch4_histogram_conf_reg = new EHistConfigRegister(0x00DC)

      /** Energy Histogram Configuration Registers */
      val histogram_conf_reg = Map(
        1 -> ch1_histogram_conf_reg, 2 -> ch2_histogram_conf_reg,
        3 -> ch3_histogram_conf_reg, 4 -> ch4_histogram_conf_reg
      )


      /** ADC FPGA Firmware Version Register
        *
        * fw_type values:
        *     * 0x0125: 125 MHz 16-bit ADC
        *     * 0x0250: 250 MHz 14-bit ADC
        */
      object firmware_reg extends RORegister[Int](0x0100) {
        val fw_type           = ROBits(16 to 31) withConv BCDConv  // Firmware Type
        val fw_version        = ROBits(8 to 15)  // Firmware Version
        val fw_revision       = ROBits(0 to 7)  // Firmware Revision
      }

      /** ADC FPGA Status Register */
      object status_reg extends RORegister[Int](0x0104) {
        val adc_clk_dcm_reset = ROBit(21)  // ADC-Clock DCM RESET flag
        val adc_clk_dcm_ok    = ROBit(20)  // ADC-Clock DCM OK flag
        val mem2_ok           = ROBit(17)  // Memory 2 OK flag (ch3 and ch4)
        val mem1_ok           = ROBit(16)  // Memory 1 OK flag (ch1 and ch2)
        val link_speed_fl     = ROBit(8)  // Data Link Speed flag
        val vme_frame_err_la  = ROBit(7)  // VME FPGA : Frame_error_latch
        val vme_soft_err_la   = ROBit(6)  // VME FPGA : Soft_error_latch
        val vme_hard_err_la   = ROBit(5)  // VME FPGA : Hard_error_latch
        val vme_lane_up_fl    = ROBit(4)  // VME FPGA : Lane_up_flag
        val vme_ch_up_fl      = ROBit(3)  // VME FPGA : Channel_up_flag
        val vme_frame_err_fl  = ROBit(2)  // VME FPGA : Frame_error_flag
        val vme_soft_err_fl   = ROBit(1)  // VME FPGA : Soft_error_flag
        val vme_hard_err_fl   = ROBit(0)  // VME FPGA : Hard_error_flag
      }


      /** Sample Address Register */
      class SampleAddrRegister(address: MemAddress) extends RORegister[Int](address){
        val ch_offset         = ROBit(25)  // Indicates the Channel Offset
        val bank              = ROBit(24) withConv ((1, 2))  // Indicates the Bank^
        val sample_addr       = ROBits(0 to 23) withConv IntMultOffsetConv(scale = 4)  // Actual Sample Address
      }


      /** Ch. 1 Actual Sample Address Register */
      val ch1_actual_sample_address_reg = new SampleAddrRegister(0x0110)

      /** Ch. 2 Actual Sample Address Register */
      val ch2_actual_sample_address_reg = new SampleAddrRegister(0x0114)

      /** Ch. 3 Actual Sample Address Register */
      val ch3_actual_sample_address_reg = new SampleAddrRegister(0x0118)

      /** Ch. 4 Actual Sample Address Register */
      val ch4_actual_sample_address_reg = new SampleAddrRegister(0x011C)

      /** Actual Sample Address Registers */
      val actual_sample_address_reg = Map(
        1 -> ch1_actual_sample_address_reg, 2 -> ch2_actual_sample_address_reg,
        3 -> ch3_actual_sample_address_reg, 4 -> ch4_actual_sample_address_reg
      )


      /** Ch. 1 Previous Bank Sample address Register */
      val ch1_previous_bank_sample_address_reg = new SampleAddrRegister(0x0120)

      /** Ch. 2 Previous Bank Sample address Register */
      val ch2_previous_bank_sample_address_reg = new SampleAddrRegister(0x0124)

      /** Ch. 3 Previous Bank Sample address Register */
      val ch3_previous_bank_sample_address_reg = new SampleAddrRegister(0x0128)

      /** Ch. 4 Previous Bank Sample address Register */
      val ch4_previous_bank_sample_address_reg = new SampleAddrRegister(0x012C)

      /** Previous Bank Sample address Registers */
      val previous_bank_sample_address_reg = Map(
        1 -> ch1_previous_bank_sample_address_reg, 2 -> ch2_previous_bank_sample_address_reg,
        3 -> ch3_previous_bank_sample_address_reg, 4 -> ch4_previous_bank_sample_address_reg
      )


      // object pps_upper_timestamp_latch_reg extends RORegister[Int](0x0130) {}
      // object pps_lower_timestamp_latch_reg extends RORegister[Int](0x0134) {}
    }

    val fpga = Map(
      1 -> new FPGARegsRegion(0x1000),
      2 -> new FPGARegsRegion(0x2000),
      3 -> new FPGARegsRegion(0x3000),
      4 -> new FPGARegsRegion(0x4000)
    )
  }

  object dataRegion extends MemRegion(0x100000, 0x500000) {
    dataRegion =>

    class FIFORegion(from: MemAddress) extends SubRegion(from, from + 0x0FFFFD) {}

    val fifo = Map(
      1 -> new FIFORegion(0x100000 - dataRegion.from),
      2 -> new FIFORegion(0x200000 - dataRegion.from),
      3 -> new FIFORegion(0x300000 - dataRegion.from),
      4 -> new FIFORegion(0x400000 - dataRegion.from)
    )


    def fpgaChFIFOAddrOffset(fpgaCh: Int, bank: Int) = {
      ( fpgaCh match {
        case 1 => 0x00000000L
        case 2 => 0x02000000L
        case 3 => 0x00000000L
        case 4 => 0x02000000L
        case _ => throw new IllegalArgumentException("ADC FPGA channel number must be between 1 and 4")
      } ) + ( bank match {
        case 1 => 0x00000000L
        case 2 => 0x01000000L
        case _ => throw new IllegalArgumentException("Bank must be 1 or 2")
      } )
    }

    val fpgaChMemSpaceSel = Map(
      1 -> registers.DataTransferCtlReg.MemSpaceSel.Mem1,
      2 -> registers.DataTransferCtlReg.MemSpaceSel.Mem1,
      3 -> registers.DataTransferCtlReg.MemSpaceSel.Mem2,
      4 -> registers.DataTransferCtlReg.MemSpaceSel.Mem2
    )
  }

  object eventFormat {
    object evtDataHdr1 extends SimpleRegister[Int] {
      val timestamp_high    = RegBits(16 to 31)  // Timestamp, high bits
      val ch_id             = RegBits( 4 to 15)  // Channel ID
      val have_energy       = RegBit(3)  // Event contains Start Energy MAW value and Max. Energy MAW value
      val have_ft_maw       = RegBit(2)  // Event contains 3 x Fast Trigger MAW values (max value, value before Trigger, value with Trigger)
      val have_acc_78       = RegBit(1)  // Event contains 2 x Accumulator values (Gates 7,8)
      val have_ph_acc16     = RegBit(0)  // Event contains Peak High values and 6 x Accumulator values (Gates 1,2, ..,6)
    }

    object evtDataHdr2 extends SimpleRegister[Int] {
      val timestamp_low     = RegBits(0 to 31)  // Timestamp, low bits
    }

    object evtDataPeakHeight extends SimpleRegister[Int] {
      val peak_heigh_idx    = RegBits(16 to 31)  // Index of Peakhigh value
      val peak_heigh_val    = RegBits( 0 to 15)  // Peakhigh value
    }

    object evtDataAccSumG1 extends SimpleRegister[Int] {
      val overflow_flag     = RegBit(24 + 7)  // Overflow flag
      val underflow_flag    = RegBit(24 + 6)  // Underflow flag
      val repileup_flag     = RegBit(24 + 5)  // RePileup flag
      val pileup_flag       = RegBit(24 + 4)  // Pileup flag
      val acc_sum_g1        = RegBits(0 to 23)  // Accumulator sum of Gate 1
    }

    object evtDataAccSum extends SimpleRegister[Int] {
      val acc_sum           = RegBits(0 to 27)  // Accumulator sum of Gate (for Gates 2 to 8)
    }

    object evtDataMAWValue extends SimpleRegister[Int] {
      val maw_val           = RegBits(0 to 27)  // MAW value (maximum or value before or after trigger)
    }

    object evtSamplesHdr extends SimpleRegister[Int] {
      val const_tag         = RegBits(28 to 31)  // Always 0xE
      val maw_test_flag     = RegBit(27)  // MAW Test Flag
      val any_pileup_flag   = RegBit(26)  // RePileup or Pileup Flag
      val n_sample_words    = RegBits(0 to 25) // number of raw samples (x 2 samples, 32-bit words)
    }
  }
}
