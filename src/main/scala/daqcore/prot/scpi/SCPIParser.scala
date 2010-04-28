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


import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.matching.Regex

import daqcore.util._
import daqcore.prot._


class SCPIParser extends ByteCharSeqParsers {
  import SCPIParser._
  
  
  lazy val nonBlockString: PackratParser[ByteCharSeq] = nonBlockStringExpr

  lazy val msgContent = (nonBlockString | string | blockData)*
  
  lazy val streamMsgRaw: PackratParser[ByteCharSeq] =
     msgContent <~ streamMsgTerm ^^ { _ match {
      case Nil => ByteCharSeq()
      case nonEmpty => nonEmpty.reduceLeft {_ ++ _}
     } }


  lazy val streamMsgTerm: PackratParser[ByteCharSeq] = streamMsgTermExpr


  protected def parseBlockData(in: Input): ParseResult[(IndexedSeq[Byte], ByteCharSeq)] = {
    val (source, offset) = (in.source, in.offset)
    if (offset >= source.length) Failure("Reached end of input", in)
    else try {
      val inSeq = in.source.asInstanceOf[ByteCharSeq]
      val input = inSeq.contents.subSequence(offset, source.length)
      if ( (input.length >= 0) && (input(0) == '#') && (input(1) >= '0') && (input(1) <= '9')) {
        val sizeLength = input(1).toChar.toString.toInt
        val dataOffset = sizeLength + 2
        if ((sizeLength <=  0) || (sizeLength > 10)) Failure("Invalid block data size length: " + sizeLength, in)
        else if (input.length < dataOffset) Failure("Block data input to short for size length " + sizeLength, in)
        else {
          val size = input.subSequence(2, dataOffset).map(_.toChar).mkString.toInt
          val dataEnd = dataOffset + size
          if (input.size < dataEnd) Failure("Block data input to short for size " + size, in)
          val data = input.subSequence(dataOffset, dataEnd)
          val raw = inSeq.subSequence(offset, offset+dataEnd)
          val rest = in.drop(dataEnd)
          require(data.length == size)
          Success((data, raw), rest)
        }
      } else Failure("Not arbitrary block data", in)
    } catch {
      case e: ClassCastException => Failure("block data can only appear in a byte-based input", in)
      case e: NumberFormatException => Failure(e.getMessage, in)
    }
  }
  
  lazy val blockDataBytes: PackratParser[IndexedSeq[Byte]] = new Parser[IndexedSeq[Byte]] {
    def apply(in: Input) = parseBlockData(in) match {
      case Success((data,all), rest) =>
        Success(data,rest)
      case f: NoSuccess => f
    }
  }

  lazy val blockData: PackratParser[ByteCharSeq] = new Parser[ByteCharSeq] {
    def apply(in: Input) = parseBlockData(in) match {
      case Success((data,raw), rest) =>
        Success(raw,rest)
      case f: NoSuccess => f
    }
  }
  
  lazy val nr1: PackratParser[ByteCharSeq] = nr1Expr
  lazy val nr2: PackratParser[ByteCharSeq] = nr2Expr
  lazy val nr3: PackratParser[ByteCharSeq] = nr3Expr
  lazy val nrf: PackratParser[ByteCharSeq] = nr3 | nr2 | nr1
  lazy val dqString: PackratParser[ByteCharSeq] = dqStringExpr
  lazy val sqString: PackratParser[ByteCharSeq] = sqStringExpr
  lazy val string: PackratParser[ByteCharSeq] = dqString | sqString

  lazy val value: PackratParser[ByteCharSeq] = skipWS (
    blockData |
    nrf | 
    nr1 |
    string
  )

  //!! missing: Suffix Program Data: ((ws?)~multiplier?)~unit

  lazy val rmu: PackratParser[Result] =
    skipWS(repsep(value, ",") ^^ { values => Result(values : _*) })
  
  lazy val response: PackratParser[Response] =
    skipWS(repsep(rmu, ";") ^^ { results => Response(results : _*) })
  
  lazy val recMnemonic: PackratParser[RecMnemonic] =
    recMnemonicExpr ^^ { mnem => RecMnemonic(mnem) }
  
  lazy val header = ccqHeader | icHeader

  lazy val ccqHeader: PackratParser[CCQHeader] =
    "*" ~> recMnemonic ^^ { mnem => CCQHeader(mnem.charSeq.toString) }
    
  lazy val mnemSuffix: PackratParser[Int] = mnemSuffixExpr ^^ { _.toString.toInt }

  lazy val icHeaderPart: PackratParser[ICHeaderPart] =
    recMnemonic ~ (mnemSuffix?) ^^
      { case mnem ~ suffix => ICHeaderPart(mnem, suffix getOrElse 1) }

  lazy val icHeader = icHeaderRel | icHeaderAbs
  
  lazy val icHeaderRel: PackratParser[ICHeaderRel] =
    repsep(icHeaderPart, ":") ^^ { parts => ICHeaderRel(parts : _*) }
  
  lazy val icHeaderAbs: PackratParser[ICHeaderAbs] =
    ":"~>icHeaderRel ^^ { rel => ICHeaderAbs(rel.parts : _*) }

  lazy val instruction = query | command
  
  lazy val query: PackratParser[Query] =
    skipWS((header<~"?") ~ (ws ~ repsep(value, ",")?)) ^^ {
      case header ~ Some(ws ~ params) => Query(header, params :_*)
      case header ~ None => Query(header)
    }

  lazy val command: PackratParser[Command] =
    skipWS(header ~ (ws ~ repsep(value, ",")?)) ^^ {
      case header ~ Some(ws ~ params) => Command(header, params :_*)
      case header ~ None => Command(header)
    }

  lazy val request: PackratParser[Request] =
    skipWS(repsep(instruction, ";") ^^ { instr => Request(instr : _*) })
    
  /** Extract a CR+LF or LF terminated message from a CharSequence */
  def extractTermMsg(in: java.lang.CharSequence) =
    streamMsgRaw(new PackratReader(new CharSequenceReader(in)))

  /** Extract a CR+LF or LF terminated message from a Reader */
  def extractTermMsg(in: Input) =  streamMsgRaw(in)
  
  def parseResponse(in: java.lang.CharSequence): Response =
    parseAll(response, in).get

  def parseHeader(in: java.lang.CharSequence): Header =
    parseAll(header, in).get

  def parseRequest(in: java.lang.CharSequence): Request =
    parseAll(request, in).get
}


object SCPIParser {
  val streamMsgTermExpr = """(\x0D?\x0A)""".r
  val nonBlockStringExpr = """(([^#'"\x0D\x0A]|[#][^0-9])+)""".r
  val nr1Expr = """-?\d+""".r
  val nr2Expr = """(\d+(\.\d*)?|\d*\.\d+)""".r
  val nr3Expr = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
  val dqStringExpr = """"([^"]*)"""".r
  val sqStringExpr = """'([^']*)'""".r
  val mnemSuffixExpr = """[1-9][0-9]*""".r
  val recMnemonicExpr = """[A-Z]+|[a-z]+""".r
  
  protected lazy val tlParser =
    { val p = new ThreadLocal[SCPIParser];  p set (new SCPIParser); p }

  def parser = tlParser.get

  def apply() = parser
}
