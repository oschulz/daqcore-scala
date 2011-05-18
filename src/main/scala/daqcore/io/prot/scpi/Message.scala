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


package daqcore.io.prot.scpi

import daqcore.util._


sealed abstract class Message extends HasByteRep


case class Response(val results: Result*) extends Message {
  def putBytes(builder: ByteSeqBuilder) = {
    var first = true
    for (x <- results) {
      if (first) {first = false} else {builder += ';'.toByte}
      x.putBytes(builder)
    }
  }
}


case class Result(values: ByteCharSeq*) extends HasByteRep {
  def putBytes(builder: ByteSeqBuilder) = {
    var first = true
    for (x <- values) {
      if (first) {first = false} else {builder += ','.toByte}
      x.putBytes(builder)
    }
  }
}


case class Request(val instr: Instruction*) extends Message {
  def putBytes(builder: ByteSeqBuilder) = {
    var first = true
    for (x <- instr) {
      if (first) {first = false} else {builder += ';'.toByte}
      x.putBytes(builder)
    }
  }
  
  def hasResponse: Boolean = instr.find(_.isInstanceOf[Query]) != None
  override def toString = instr.map(_.toString).mkString("; ")
}


sealed abstract class Instruction extends HasByteRep {
  def params: Seq[ByteCharSeq]
}


case class Command(header: Header, params: ByteCharSeq*) extends Instruction {
  def putBytes(builder: ByteSeqBuilder) = {
    header.putBytes(builder)
    if (!params.isEmpty) {
      builder += ' '.toByte
      Result(params:_*).putBytes(builder)
    }
  }
  override def toString = {
    if (!params.isEmpty) header.toString + " " + Result(params:_*).toString
    else header.toString
  }
}


case class Query(header: Header, params: ByteCharSeq*) extends Instruction {
  def putBytes(builder: ByteSeqBuilder) = {
    header.putBytes(builder)
    builder += '?'.toByte
    if (!params.isEmpty) {
      builder += ' '.toByte
      Result(params:_*).putBytes(builder)
    }
  }
  override def toString = {
    if (!params.isEmpty) header.toString + "? " + Result(params:_*).toString
    else header.toString + "?"
  }
}
