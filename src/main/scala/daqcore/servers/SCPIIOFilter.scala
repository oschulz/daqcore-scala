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

import java.io.{File}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._
import daqcore.prot.scpi.{Request, Response, SCPIParser}


trait SCPIRequestOutputFilter extends OutputFilterServer with SCPIRequestOutput {
  override def target: RawMsgOutput
  val targetCompanion = RawMsgOutput

  protected def srvSend(request: Request): Unit = {
    target.send(request.getBytes)
  }
}


object SCPIRequestOutputFilter {
  def apply(output: RawMsgOutput): SCPIRequestOutputFilter = {
    start(new SCPIRequestOutputFilter { val target = output })
  }

  def apply(file: File, compression: Compression = Uncompressed): SCPIRequestOutputFilter =
    SCPIRequestOutputFilter(GPIBStreamOutput(file, compression))
}



trait SCPIRequestInputFilter extends InputFilterServer with SCPIRequestInput {
  override def source: RawMsgInput
  val sourceCompanion = RawMsgInput

  var parser: SCPIParser = null
  
  override def init() = {
    parser = new SCPIParser
    super.init
  }
  
  protected def srvProcessInput(data: Seq[Byte]) = {
    trace("Received %s bytes: %s".format(data.size, loggable(data)))
    
    val request = parser.parseRequest(ByteCharSeq(data: _*))
    trace("Received request: %s".format(loggable(request.toString)))
    Seq(request)
  }
}


object SCPIRequestInputFilter {
  def apply(output: RawMsgInput): SCPIRequestInputFilter = {
    start(new SCPIRequestInputFilter { val source = output })
  }

  def apply(file: File, compression: Compression = Uncompressed): SCPIRequestInputFilter =
    SCPIRequestInputFilter(GPIBStreamInput(file, compression))
}



trait SCPIResponseInputFilter extends InputFilterServer with SCPIResponseInput {
  override def source: RawMsgInput
  val sourceCompanion = RawMsgInput

  var parser: SCPIParser = null
  
  override def init() = {
    parser = new SCPIParser
    super.init
  }
  
  protected def srvProcessInput(data: Seq[Byte]) = {
    trace("Received %s bytes: %s".format(data.size, loggable(data)))
    
    val response = parser.parseResponse(ByteCharSeq(data: _*))
    trace("Received response: %s".format(loggable(response.toString)))
    Seq(response)
  }
}


object SCPIResponseInputFilter {
  def apply(output: RawMsgInput): SCPIResponseInputFilter = {
    start(new SCPIResponseInputFilter { val source = output })
  }

  def apply(file: File, compression: Compression = Uncompressed): SCPIResponseInputFilter =
    SCPIResponseInputFilter(GPIBStreamInput(file, compression))
}
