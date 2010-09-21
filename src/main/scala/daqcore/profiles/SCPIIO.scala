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


package daqcore.profiles

import scala.annotation._

import daqcore.util._
import daqcore.actors._
import daqcore.prot.scpi._


trait SCPIRequestOutput extends GenericOutput { val outputCompanion = SCPIRequestOutput }
object SCPIRequestOutput extends GenericOutputCompanion { type OutputData = Request }

trait SCPIRequestInput extends GenericInput { val inputCompanion = SCPIRequestInput }
object SCPIRequestInput extends GenericInputCompanion { type InputData = Request }

trait SCPIResponseOutput extends GenericOutput { val outputCompanion = SCPIResponseOutput }
object SCPIResponseOutput extends GenericOutputCompanion { type OutputData = Response }

trait SCPIResponseInput extends GenericInput { val inputCompanion = SCPIResponseInput }
object SCPIResponseInput extends GenericInputCompanion { type InputData = Response }


// If an Server implementing SCPIClientIO is sent a Request object, it
// will automatically reply if the Request has a response. If you want to
// pull the response yourself, use Send(Request) / Recv() messages.
trait SCPIClientIO extends SCPIRequestOutput with SCPIResponseInput {
  def queryF(instr: Instruction*)(implicit timeout: TimeoutSpec): Ft[Response] = {
    val request = Request(instr: _*)
    require(request.hasResponse)
    srv.!!? (Request)(timeout) map
      { case x: Response => x }
  }
  
  def cmd(instr: Instruction*) : Unit = {
    val request = Request(instr: _*)
    require(! request.hasResponse)
    srv ! request
  }
}

object SCPIClientIO {
  /*def apply(io: RawMsgIO) = SCPIClientIOSrv(io)

  def apply(host: String, device: String) = SCPIClientIOSrv(VXI11ClientLink(host, device))

  def apply(io: ByteStreamIO) = SCPIClientIOSrv(io)

  def apply(host: String, port: Int) = SCPIClientIOSrv(GPIBStreamIO(InetConnection(host, port)))*/
}
