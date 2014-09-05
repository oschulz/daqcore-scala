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


trait COVar[A, +B <: COAccessType[A]] extends ObjDirEntry {
  def objectType = 0x07
  def defaultValue: Option[A]
  def pdoMapping: Boolean
  def access: B with COAccessType[A]

  def dataType = access.dataType
  def isReadable = access.isReadable
  def isWritable = access.isWriteable
  def isConst = access.isConst
}


trait COObject extends ObjDirEntry {
  val index: Int
  val subIndex = 0
  val name: String
}


case class COVarObject[A, +B <: COAccessType[A]]
  (index: Int, name: String, access: B with COAccessType[A], defaultValue: Option[A] = None, pdoMapping: Boolean = false)
  extends COVar[A, B] with COObject



abstract class COComplexObject[+S <: COSize] extends COObject {
  parent =>

  def sizeType: S with COSize
  def defaultSize: Option[Int]

  protected def checkSize(size: Int) {
    if ((size < 0) || (size > 0xff))
	  throw new IllegalArgumentException("Illegal number of entries for CANopen complex object: " + subIndex)
  }

  trait SubObject extends ObjDirEntry {
    val index = parent.index
  }

  case object size extends COVar[Short, S] with SubObject {
	val subIndex = 0
	val name = "nEntries"
    final def access = parent.sizeType
    final def defaultValue = parent.defaultSize map {_.toShort}
    final def pdoMapping = false
  }
}



case class COArray[A, +B <: COAccessType[A], +S <: COSize] (
    index: Int, name: String, access: B with COAccessType[A], defaultValue: Option[A] = None, pdoMapping: Boolean = false,
    sizeType: S with COSize = ROSize, defaultSize: Option[Int] = None, maxSize: Int = 0xff
) extends COComplexObject[S] {
  array =>

  final def objectType = 0x08

  defaultSize foreach checkSize
  checkSize(maxSize)

  def isReadable = access.isReadable
  def isWritable = access.isWriteable
  def isConst = access.isConst

  case class Element(subIndex: Int, name: String) extends COVar[A, B] with SubObject {
    final def access = array.access
    final def defaultValue = array.defaultValue
    final def pdoMapping = array.pdoMapping
  }

  def apply(i: Int): Element = Element(i, i.toString)
}


abstract class CORecord(val index: Int, val name: String) extends COComplexObject[COTypedConst[Short]] {
  def objectType = 0x09

  val sizeType = ConstSize
  lazy val defaultSize = Some((members map {_.subIndex}).max)

  protected case class Member[A, +B <: COAccessType[A]]
    (subIndex: Int, name: String, access: B with COAccessType[A], defaultValue: Option[A] = None, pdoMapping: Boolean = false)
    extends COVar[A, B] with SubObject

  lazy val membersByName: Map[String, COVarAny] = (for {
    method <- this.getClass.getDeclaredMethods.toSet
    if method.getParameterTypes.isEmpty
    if classOf[Member[_, _]].isAssignableFrom(method.getReturnType)
  } yield {
    method.getName -> method.invoke(this).asInstanceOf[COVarAny]
  }).toMap

  lazy val membersByIdx: Map[Int, COVarAny] = membersByName map { case (k, m) => m.subIndex -> m }

  lazy val members: Seq[COVarAny] = membersByName.values.toList sortWith { (a,b) => a.subIndex < b.subIndex }

  def apply(i: Int) = membersByIdx(i)
  def apply(name: String) = membersByName(name)
}
