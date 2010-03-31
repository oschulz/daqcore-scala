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


object NR1 {
  def apply(value: Int) = ByteCSeq(value.toString)

  def unapply(bs: ByteCSeq) : Option[Int] =
    try { Some(bs.toString.toInt) } catch { case e: NumberFormatException => None }
}


object NRf {
  def apply(value: Double) = ByteCSeq(value.toString)

  def unapply(bs: ByteCSeq) : Option[Double] =
    try { Some(bs.toString.toDouble) } catch { case e: NumberFormatException => None }
}


object SRD {
  protected val sqString = """'([^']*)'""".r
  protected val dqString = """"([^']*)"""".r

  def apply(value: String) = ByteCSeq("\"") ++ ByteCSeq(value) ++ ByteCSeq("\"")

  def unapply(bs: ByteCSeq) : Option[String] = bs match {
    case sqString(contents) => Some(contents)
    case dqString(contents) => Some(contents)
    case _ => None
  }
}


object BlockData {
  def apply(data: IndexedSeq[Byte]) : ByteCSeq = {
    val tag = "#".getBytes
    val sizeStr = data.size.toString.getBytes
    val sizeSizeStr = sizeStr.size.toString.getBytes
    
    ByteCSeq(tag ++ sizeSizeStr ++ sizeStr ++ data)
  }


  def unapply(bs: ByteCSeq) : Option[IndexedSeq[Byte]] = try {
    val parser = SCPIParser()
    Some(parser.parseAll(parser.blockDataBytes, bs).get)
  } catch {
    case _ => None
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


case class Response(val results: Result*) {
  def charSeq: ByteCSeq = results.toList match {
    case Nil => ByteCSeq("")
    case head::Nil => head.charSeq
    case head::tail => tail.foldLeft(head.charSeq) { (bs, r) => bs ++ ByteCSeq(";") ++ r.charSeq }
  }
}
