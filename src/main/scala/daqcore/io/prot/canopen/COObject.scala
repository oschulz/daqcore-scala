// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.io.prot.canopen

import daqcore.util._


trait ObjDirEntry {
  val index: Int
  val subIndex: Int
  def objectType: Int
  val name: String

  require ((index >= 0) && (index <= 0xffff))
  require ((subIndex >= 0) && (subIndex <= 0xff))
}



trait COAccessType[A] {
  def dataType: CODataType[A]
  def isReadable: Boolean = false
  def isWriteable: Boolean = false
  def isConst: Boolean = false
}


trait COReadable[A] extends COAccessType[A] {
  override def isReadable = true
  def dataType: CODataType[A]

  def decode(iterator: ByteIterator) = dataType.decode(iterator)
  def decode(bytes: ByteString): A = decode(bytes.iterator)
}


trait COWriteable[A] extends COAccessType[A] {
  override def isWriteable = true
  def dataType: CODataType[A]

  def encode(builder: ByteStringBuilder, value: A) = dataType.encode(builder, value)
  def encode(value: A): ByteString = encode(ByteString.newBuilder, value).result
}



trait COObject extends ObjDirEntry {
  val index: Int
  val subIndex = 0
  val name: String
}



trait COVariable[A] extends ObjDirEntry with COAccessType[A] {
  def objectType = 0x07
  def defaultValue: Option[A]
  def pdoMapping: Boolean
}


trait COReadableVariable[A] extends COVariable[A] with COReadable[A]
trait COWritableVariable[A] extends COVariable[A] with COWriteable[A]

trait COAccessRO[A] extends COReadableVariable[A]
trait COAccessWO[A] extends COWritableVariable[A]
trait COAccessRW[A] extends COReadableVariable[A] with COWritableVariable[A]
trait COAccessConst[A] extends COReadableVariable[A] { override def isConst = false }



trait COObjectVariable[A] extends COVariable[A] with COObject

case class COVariableRO[A](index: Int, name: String, dataType: CODataType[A], defaultValue: Option[A] = None, pdoMapping: Boolean = false) extends COObjectVariable[A] with COAccessRO[A]
case class COVariableWO[A](index: Int, name: String, dataType: CODataType[A], defaultValue: Option[A] = None, pdoMapping: Boolean = false) extends COObjectVariable[A] with COAccessWO[A]
case class COVariableRW[A](index: Int, name: String, dataType: CODataType[A], defaultValue: Option[A] = None, pdoMapping: Boolean = false) extends COObjectVariable[A] with COAccessRW[A]
case class COVariableConst[A](index: Int, name: String, dataType: CODataType[A], defaultValue: Option[A] = None, pdoMapping: Boolean = false) extends COObjectVariable[A] with COAccessConst[A]



trait COComplexObject extends COObject {
  parent =>

  def rwSize: Boolean
  def defaultSize: Option[Int]

  protected def checkSize(size: Int) {
	  if ((size < 0) || (size > 0xff))
		throw new IllegalArgumentException("Illegal number of entries for CANopen complex object: " + subIndex)
  }

  defaultSize foreach checkSize

  trait SubObject extends ObjDirEntry {
    val index = parent.index
  }

  trait Member[A] extends COVariable[A] with SubObject

  case class MemberRO[A](subIndex: Int, name: String, dataType: CODataType[A], defaultValue: Option[A] = None, pdoMapping: Boolean = false) extends Member[A] with COAccessRO[A]
  case class MemberWO[A](subIndex: Int, name: String, dataType: CODataType[A], defaultValue: Option[A] = None, pdoMapping: Boolean = false) extends Member[A] with COAccessWO[A]
  case class MemberRW[A](subIndex: Int, name: String, dataType: CODataType[A], defaultValue: Option[A] = None, pdoMapping: Boolean = false) extends Member[A] with COAccessRW[A]
  case class MemberConst[A](subIndex: Int, name: String, dataType: CODataType[A], defaultValue: Option[A] = None, pdoMapping: Boolean = false) extends Member[A] with COAccessConst[A]

  val size: COReadableVariable[Short] = {
	val n = defaultSize map {_.toShort}
    if (rwSize) MemberRW[Short](0, "nEntries", COUnsigned8, n, false)
    else MemberRO[Short](0, "nEntries", COUnsigned8, n, false)
  }
}



trait COArray[A, B <: COVariable[A]] extends COComplexObject with COAccessType[A] {
  array =>

  final def objectType = 0x08

  def dataType: CODataType[A]
  def defaultValue: Option[A]
  def pdoMapping: Boolean
  def maxSize: Int
  
  checkSize(maxSize)

  abstract class GenericElement protected extends Member[A] {
    if ((subIndex < 1) || (subIndex > maxSize)) throw new IndexOutOfBoundsException("Illegal sub-index for CANopen array element: " + subIndex)
    final def dataType = array.dataType
    final def defaultValue = array.defaultValue
    final def pdoMapping = array.pdoMapping
  }

  def apply(i: Int): GenericElement
}

trait COReadableArray[A, B <: COReadableVariable[A]] extends COArray[A, B]
trait COWritableArray[A, B <: COWritableVariable[A]] extends COArray[A, B]

case class COArrayRO[A](
    index: Int, name: String, dataType: CODataType[A],
    maxSize: Int = 0xff, defaultSize: Option[Int] = None, rwSize: Boolean = false,
    defaultValue: Option[A] = None, pdoMapping: Boolean = false
) extends COReadableArray[A, COAccessRO[A]] {
  case class Element protected[COArrayRO] (subIndex: Int, name: String) extends GenericElement with COAccessRO[A]
  def apply(i: Int): Element = Element(i, i.toString)
}

case class COArrayWO[A](
    index: Int, name: String, dataType: CODataType[A],
    maxSize: Int = 0xff, defaultSize: Option[Int] = None, rwSize: Boolean = false,
    defaultValue: Option[A] = None, pdoMapping: Boolean = false
) extends COWritableArray[A, COAccessWO[A]] {
  case class Element protected[COArrayWO] (subIndex: Int, name: String) extends GenericElement with COAccessWO[A]
  def apply(i: Int): Element = Element(i, i.toString)
}

case class COArrayRW[A](
    index: Int, name: String, dataType: CODataType[A],
    maxSize: Int = 0xff, defaultSize: Option[Int] = None, rwSize: Boolean = false,
    defaultValue: Option[A] = None, pdoMapping: Boolean = false
) extends COReadableArray[A, COAccessRW[A]] with COWritableArray[A, COAccessRW[A]] {
  case class Element protected[COArrayRW] (subIndex: Int, name: String) extends GenericElement with COAccessRW[A]
  def apply(i: Int): Element = Element(i, i.toString)
}

case class COArrayConst[A](
    index: Int, name: String, dataType: CODataType[A],
    maxSize: Int = 0xff, defaultSize: Option[Int] = None, rwSize: Boolean = false,
    defaultValue: Option[A] = None, pdoMapping: Boolean = false
) extends COReadableArray[A, COAccessConst[A]] {
  case class Element protected[COArrayConst] (subIndex: Int, name: String) extends GenericElement with COAccessConst[A]
  def apply(i: Int): Element = Element(i, i.toString)
}



abstract class CORecord(val index: Int, val name: String) extends COComplexObject {
  def objectType = 0x09

  def rwSize = false
  def defaultSize = None
}
