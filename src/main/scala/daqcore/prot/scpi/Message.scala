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


sealed abstract class Message extends ByteCharSeqFragment


case class Response(val results: Result*) extends Message {
  def charSeq: ByteCharSeq = results.toList match {
    case Nil => ByteCharSeq("")
    case head::Nil => head.charSeq
    case head::tail => tail.foldLeft(head.charSeq) { (bs, r) => bs ++ ByteCharSeq(";") ++ r.charSeq }
  }
}


case class Result(values: ByteCharSeq*) {
  def charSeq: ByteCharSeq = values.toList match {
    case Nil => ByteCharSeq("")
    case head::Nil => head
    case head::tail => tail.foldLeft(head) { (bs, v) => bs ++ ByteCharSeq(",") ++ v }
  }
  
  def +(seq: Seq[ByteCharSeq]): Result = Result(values ++ seq : _*)
}


case class Request(val instr: Instruction*) extends Message {
  def charSeq: ByteCharSeq = instr.toList match {
    case Nil => ByteCharSeq("")
    case head::Nil => head.charSeq
    case head::tail => tail.foldLeft(head.charSeq) { (bs, r) => bs ++ ByteCharSeq(";") ++ r.charSeq }
  }
  
  def hasResponse: Boolean = instr.find(_.isInstanceOf[Query]) != None
  override def toString = instr.map(_.toString).mkString("; ")
}


sealed abstract class Instruction extends ByteCharSeqFragment {
  def params: Seq[ByteCharSeq]
}


case class Command(header: Header, params: ByteCharSeq*) extends Instruction {
  def charSeq = header.charSeq ++ {
    if (!params.isEmpty) ByteCharSeq(" ") ++ Result(params:_*).charSeq
    else ByteCharSeq("")
  }
  def ? = Query(header, params:_*)
  override def toString = header.toString + {
    if (!params.isEmpty) " " + params.map(_.toString).mkString(", ")
    else ""
  }
}


case class Query(header: Header, params: ByteCharSeq*) extends Instruction {
  def charSeq = header.charSeq ++ {
    if (!params.isEmpty) ByteCharSeq("? ") ++ Result(params:_*).charSeq
    else ByteCharSeq("?")
  }
  def ? = Query(header, params:_*)
  override def toString = header.toString + {
    if (!params.isEmpty) "? " + params.map(_.toString).mkString(", ")
    else "?"
  }
}
