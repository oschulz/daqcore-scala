// Copyright (C) 2011 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.io.prot

import scala.language.implicitConversions


package object snmp {


import daqcore.util._

import java.net.InetAddress


type VariableBindings = Seq[(OID, SMIValue)]


implicit def int2SMI(x: Int) = Integer32(x)
implicit def smi2Int(x: Integer32) = x.toInt

implicit def string2SMI(x: String) = OctetString(x)
implicit def smi2String(x: OctetString) = x.toString

implicit def iaddr2SMI(x: InetAddress) = IpAddress(ByteString(x.getAddress))
implicit def smi2iaddr2SMI(x: IpAddress) = InetAddress.getByAddress(x.toByteString.toArray)

implicit def float2SMI(x: Float) = OpaqueFloat(x)
implicit def smi2Float(x: OpaqueFloat) = x.toFloat

implicit def double2SMI(x: Double) = OpaqueDouble(x)
implicit def smi2Double(x: OpaqueDouble) = x.toDouble


}
