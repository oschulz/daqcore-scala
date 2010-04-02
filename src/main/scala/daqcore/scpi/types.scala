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


trait SCPIFragment {
  def charSeq: ByteCSeq
}

sealed abstract class Message extends SCPIFragment


case class Response(val results: Result*) extends Message {
  def charSeq: ByteCSeq = results.toList match {
    case Nil => ByteCSeq("")
    case head::Nil => head.charSeq
    case head::tail => tail.foldLeft(head.charSeq) { (bs, r) => bs ++ ByteCSeq(";") ++ r.charSeq }
  }
}


case class Request(val instr: Instruction*) extends Message {
  def charSeq: ByteCSeq = instr.toList match {
    case Nil => ByteCSeq("")
    case head::Nil => head.charSeq
    case head::tail => tail.foldLeft(head.charSeq) { (bs, r) => bs ++ ByteCSeq(";") ++ r.charSeq }
  }
  
  def hasResponse: Boolean = instr.find(_.isInstanceOf[Query]) != None
}


sealed abstract class Mnemonic extends SCPIFragment {
}


object Mnemonic {
  val mnemExp = """^([A-Z]+)([a-z]+)?$""".r
  
  def apply(mnem: String): SpecMnemonic = mnem match {
    case mnemExp(short, rest) => SpecMnemonic(short, if (rest!=null) short+rest else short)
    case _ => throw new IllegalArgumentException("Not a valid mnemonic: \"%s\"".format(mnem))
  }
}
  
case class RecMnemonic(charSeq: ByteCSeq) extends Mnemonic {
  override def hashCode = charSeq.toString.hashCode
    override def canEqual(that: Any) = that.isInstanceOf[Mnemonic]
    override def equals(that: Any) = canEqual(that) && (that match {
    case that:RecMnemonic => that.charSeq == this.charSeq
    case that:SpecMnemonic => that == this
    case _ => false
  })
}


case class SpecMnemonic(short:String, long:String) extends Mnemonic {
  def charSeq = ByteCSeq(long)

  override def hashCode = short.hashCode
    def canEqual(that: Any) = that.isInstanceOf[Mnemonic]
    override def equals(that: Any) = canEqual(that) && (that match {
    case that:RecMnemonic => {
      val thatStr = that.charSeq.toString
      ( (thatStr == short.toLowerCase) || (thatStr == long.toLowerCase) ||
      (thatStr == short.toUpperCase) || (thatStr == long.toUpperCase) )
    }
    case that:SpecMnemonic => ((that.short == this.short) && (that.long == this.long))
  })
  
  def apply(suffix: Int) = ICHeaderPart(this, suffix)
  def unapply(part: ICHeaderPart): Option[Int] = {
    if (part.mnem == this) Some(part.suffix)
    else None
  }
}


sealed abstract class Header extends SCPIFragment {
  def ! = Command(this)
  def !(params: ByteCSeq*) = Command(this, params: _*)
  def ? = Query(this)
  def ?(params: ByteCSeq*) = Query(this, params: _*)
}


case class CCQHeader(mnemonic: String) extends Header {
  require(mnemonic == mnemonic.toUpperCase)
  def charSeq = ByteCSeq("*") ++ ByteCSeq(mnemonic)
}


sealed abstract class ICHeader extends Header {
  def ~(part: ICHeaderPart): ICHeader
}


case class ICHeaderAbs(parts: ICHeaderPart*) extends ICHeader {
  def charSeq = ByteCSeq(":") ++ parts.map(_.charSeq).reduceLeft(_ ++ ByteCSeq(":") ++ _)
  def ~(part: ICHeaderPart): ICHeaderAbs = ICHeaderAbs((parts ++ Seq(part)) :_*)
}

case class ICHeaderRel(parts: ICHeaderPart*) extends ICHeader {
  def charSeq = parts.map(_.charSeq).reduceLeft(_ ++ ByteCSeq(":") ++ _)
  def unary_~ = ICHeaderAbs(parts :_*)
  def ~(part: ICHeaderPart): ICHeaderRel = ICHeaderRel((parts ++ Seq(part)) :_*)
}


case class ICHeaderPart(mnem: Mnemonic, suffix: Int = 1) {
  require(suffix >= 1)
  def charSeq = mnem.charSeq ++ (if (suffix > 1) ByteCSeq(suffix.toString) else ByteCSeq(""))
  def unary_~ = ICHeaderAbs(this)
  def ~(that: ICHeaderPart) = ICHeaderRel(this, that)
}


sealed abstract class Instruction extends SCPIFragment {
  def params: Seq[ByteCSeq]
}


case class Command(header: Header, params: ByteCSeq*) extends Instruction {
  def charSeq = header.charSeq ++ ByteCSeq(" ") ++ Result(params:_*).charSeq
  def ? = Query(header, params:_*)
}

case class Query(header: Header, params: ByteCSeq*) extends Instruction {
  def charSeq = header.charSeq ++ ByteCSeq("? ") ++ Result(params:_*).charSeq
}
