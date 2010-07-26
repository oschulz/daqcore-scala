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


package daqcore.prot.scpi

import daqcore.util._


sealed abstract class Message extends HasByteRep


case class Response(val results: Result*) extends Message {
  def getBytes = results map {_.getBytes} flatWithSep ByteCharSeq(";")
}


case class Result(values: ByteCharSeq*) {
  def getBytes = values flatWithSep ByteCharSeq(",")
}


case class Request(val instr: Instruction*) extends Message {
  def getBytes = instr map {_.getBytes} flatWithSep ByteCharSeq(";")
  
  def hasResponse: Boolean = instr.find(_.isInstanceOf[Query]) != None
  override def toString = instr.map(_.toString).mkString("; ")
}


sealed abstract class Instruction extends HasByteRep {
  def params: Seq[ByteCharSeq]
}


case class Command(header: Header, params: ByteCharSeq*) extends Instruction {
  def getBytes = {
    if (!params.isEmpty) Seq(header.getBytes, ByteCharSeq(" "), Result(params:_*).getBytes).flat
    else header.getBytes
  }
  override def toString = {
    if (!params.isEmpty) header.toString + " " + Result(params:_*).toString
    else header.toString
  }
}


case class Query(header: Header, params: ByteCharSeq*) extends Instruction {
  def getBytes = {
    if (!params.isEmpty) Seq(header.getBytes, ByteCharSeq("? "), Result(params:_*).getBytes).flat
    else Seq(header.getBytes, ByteCharSeq("?")).flat
  }
  override def toString = {
    if (!params.isEmpty) header.toString + "? " + Result(params:_*).toString
    else header.toString + "?"
  }
}
