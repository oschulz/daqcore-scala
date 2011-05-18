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


package daqcore.io.prot.snmp

import java.net.URL


case class MIBObject (
  oid: OID,
  name: String,
  readable: Boolean,
  writeable: Boolean,
  units: String = "",
  description: String = ""
)



case class MIB(mibURL: URL) {
  import net.percederberg.mibble.{MibLoader, MibSymbol, MibValue, MibValueSymbol}
  import net.percederberg.mibble.snmp.SnmpObjectType
  import net.percederberg.mibble.value.ObjectIdentifierValue
  import net.percederberg.mibble.snmp.SnmpAccess._
  import scala.collection.JavaConversions._

  protected val loader = new MibLoader()
  protected val mib = loader.load(mibURL)

  protected final def oidparts(oid: ObjectIdentifierValue, xs: List[Int] = Nil): List[Int] = oid match {
    case null => xs
    case oid => oidparts(oid.getParent, oid.getValue::xs)
  }

  protected def mibObject(sym: MibValueSymbol): MIBObject = {
    val oidval = sym.getValue.asInstanceOf[ObjectIdentifierValue]
    val otype = sym.getType.asInstanceOf[SnmpObjectType]
    val oid = OID(oidparts(oidval): _*)
    MIBObject(
      oid = oid,
      name = sym.getName,
      readable = otype.getAccess match { case READ_ONLY | READ_WRITE  => true; case _ => false }, // How to handle READ_CREATE?
      writeable = otype.getAccess match { case READ_WRITE | WRITE_ONLY => true; case _ => false }, // How to handle READ_CREATE?
      units = otype.getUnits match { case null => ""; case s => s },
      description = otype.getDescription match { case null => ""; case s => s }
      // otype.getStatus
      // sym.isTableColumn
      // sym.isTableRow
    )
  }

  val symbols = {
    val syms = mib.getAllSymbols flatMap { _ match {
      case sym: MibValueSymbol if sym.getType.isInstanceOf[SnmpObjectType] => {
        val obj = mibObject(sym)
        Some((obj.name, obj))
      }
      case _ => None
    } }
    syms.toMap
  }
}
