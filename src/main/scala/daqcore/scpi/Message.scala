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


package daqcore.scpi

import daqcore.util._


sealed abstract class Message extends SCPIFragment


case class Response(val results: Result*) extends Message {
  def charSeq: ByteCSeq = results.toList match {
    case Nil => ByteCSeq("")
    case head::Nil => head.charSeq
    case head::tail => tail.foldLeft(head.charSeq) { (bs, r) => bs ++ ByteCSeq(";") ++ r.charSeq }
  }
}


case class Result(values: ByteCSeq*) {
  def charSeq: ByteCSeq = values.toList match {
    case Nil => ByteCSeq("")
    case head::Nil => head
    case head::tail => tail.foldLeft(head) { (bs, v) => bs ++ ByteCSeq(",") ++ v }
  }
  
  def +(seq: Seq[ByteCSeq]): Result = Result(values ++ seq : _*)
}


case class Request(val instr: Instruction*) extends Message {
  def charSeq: ByteCSeq = instr.toList match {
    case Nil => ByteCSeq("")
    case head::Nil => head.charSeq
    case head::tail => tail.foldLeft(head.charSeq) { (bs, r) => bs ++ ByteCSeq(";") ++ r.charSeq }
  }
  
  def hasResponse: Boolean = instr.find(_.isInstanceOf[Query]) != None
  override def toString = instr.map(_.toString).mkString("; ")
}


sealed abstract class Instruction extends SCPIFragment {
  def params: Seq[ByteCSeq]
}


case class Command(header: Header, params: ByteCSeq*) extends Instruction {
  def charSeq = header.charSeq ++ {
    if (!params.isEmpty) ByteCSeq(" ") ++ Result(params:_*).charSeq
    else ByteCSeq("")
  }
  def ? = Query(header, params:_*)
  override def toString = header.toString + {
    if (!params.isEmpty) " " + params.map(_.toString).mkString(", ")
    else ""
  }
}


case class Query(header: Header, params: ByteCSeq*) extends Instruction {
  def charSeq = header.charSeq ++ {
    if (!params.isEmpty) ByteCSeq("? ") ++ Result(params:_*).charSeq
    else ByteCSeq("?")
  }
  def ? = Query(header, params:_*)
  override def toString = header.toString + {
    if (!params.isEmpty) "? " + params.map(_.toString).mkString(", ")
    else "?"
  }
}
