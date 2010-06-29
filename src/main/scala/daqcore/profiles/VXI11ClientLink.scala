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

import java.net.InetAddress

import daqcore.util._
import daqcore.actors._


trait VXI11ClientLink extends MsgIO {
  // def lock(flags: Long = 0, timeout: Long = -1) =
  //  srv.!!?> (VXI11ClientLink.Lock(flags, timeout)) { case x: Boolean => x }

  // def unlock() = srv ! VXI11ClientLink.Unlock

  // def clear() = srv !!? VXI11ClientLink.Clear
}


object VXI11ClientLink {
  def apply(host: String, device:String)(implicit connector: VXI11Connector): VXI11ClientLink =
    connector.connectF(host, device)()

  def apply(host: String, device:String, timeout: Long)(implicit connector: VXI11Connector): VXI11ClientLink =
    connector.connectF(host, device, timeout)()
  
  def apply(to: InetAddress, device:String)(implicit connector: VXI11Connector): VXI11ClientLink =
    connector.connectF(to, device)()

  def apply(to: InetAddress, device:String, timeout: Long)(implicit connector: VXI11Connector): VXI11ClientLink =
    connector.connectF(to, device, timeout)()

  // Not supported yet: case class Lock(flags: Int = 0, timeout: Long = -1) // Reply: Boolean
  // Not supported yet: case object Unlock // No Reply
  // Not supported yet: case object Clear // Reply: Unit
  // Not supported yet: case object Abort
  // Not supported yet: case object ReadStatus // Reply: Byte
  // Not supported yet: device_trigger, device_remote, device_local
  // Not supported yet: device_enable_srq, device_docmd
  // Not supported yet: create_intr_chan, destroy_intr_chan
  // Not supported yet: DEVICE_ASYNC.device_abort
}
