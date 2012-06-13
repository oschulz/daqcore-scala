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

import java.net.InetAddress

import daqcore.util._

sealed trait SMIValue


case class Null() extends SMIValue    


case class OID(values: Int*) extends SMIValue with Ordered[OID] {
  def ~(i: Int): OID = OID((values :+ i): _*)
  
  def compare(that: OID) = {
    var res = 0
    val (a,b) = (this.values.iterator, that.values.iterator)
    while ((res == 0) && a.hasNext && b.hasNext) {
      val (i,j) = (a.next, b.next)
      if (i > j) res = 1 else if (i < j) res = -1
    }
    if (res != 0) res else if (a.hasNext) 1 else if(b.hasNext) -1 else 0
  }
  
  def isChildOf(that: OID) = (this.values startsWith that.values) && (this.values.length > that.values.length)

  override def toString = values.mkString(".")
  
  def this(values: String) = this(values.split('.') map {_.toInt}: _*)
}

case object OID {
  def apply(values: String): OID = new OID(values)
}


case class Integer32(value: Int) extends SMIValue { def toInt = value }

case class Unsigned32(value: Long) extends SMIValue { def toLong = value }

case class Counter32(value: Long) extends SMIValue { def toLong = value }

case class Counter64(value: Long) extends SMIValue { def toLong = value }

case class Gauge32(value: Long) extends SMIValue { def toLong = value }

case class TimeTicks(value: Long) extends SMIValue { def toLong = value }

case class OctetString(value: String) extends SMIValue

case class IpAddress(value: ByteString) extends SMIValue {
  require(value.length == 4)
  def toByteString = value
  override def toString = value map {b: Byte => b.toInt & 0xff} mkString "."
}

case class OpaqueData(value: ByteString) extends SMIValue {
  override def toString = "OpaqueData(%s)".format(value map hex mkString " ")
}

case class OpaqueFloat(value: Float) extends SMIValue {
  def toFloat = value
}

case class OpaqueDouble(value: Double) extends SMIValue {
  def toDouble = value
}
