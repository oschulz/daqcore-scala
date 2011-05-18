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


import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.matching.Regex

import daqcore.util._
import daqcore.io.prot._


class SCPIParser extends ByteCharSeqParsers {
  import SCPIParser._
  
  def nonBlockString: Parser[ByteCharSeq] = nonBlockStringExpr

  def msgContent = (nonBlockString | string | blockData)*
  
  def streamMsgRaw: Parser[ByteCharSeq] =
     msgContent <~ streamMsgTerm ^^ { _ match {
      case Nil => ByteCharSeq()
      case nonEmpty => nonEmpty.reduceLeft {_ ++ _}
     } }


  def streamMsgTerm: Parser[ByteCharSeq] = streamMsgTermExpr


  protected def parseBlockData(in: Input): ParseResult[(ByteSeq, ByteCharSeq)] = {
    val (source, offset) = (in.source, in.offset)
    if (offset >= source.length) Failure("Reached end of input", in)
    else try {
      val inSeq = in.source.asInstanceOf[ByteCharSeq]
      val input = inSeq.subSequence(offset, source.length)
      if ( (input.length >= 0) && (input(0) == '#') && (input(1) >= '0') && (input(1) <= '9')) {
        val sizeLength = input(1).toChar.toString.toInt
        val dataOffset = sizeLength + 2
        if ((sizeLength <=  0) || (sizeLength > 10)) Failure("Invalid block data size length: " + sizeLength, in)
        else if (input.length < dataOffset) Failure("Block data input to short for size length " + sizeLength, in)
        else {
          val size = input.subSequence(2, dataOffset).map(_.toChar).mkString.toInt
          val dataEnd = dataOffset + size
          if (input.size < dataEnd) Failure("Block data input to short for size " + size, in)
          else {
            val data = input.subSequence(dataOffset, dataEnd)
            val raw = inSeq.subSequence(offset, offset+dataEnd)
            val rest = in.drop(dataEnd)
            require(data.length == size)
            Success((ByteSeq.wrap(data.toArray), raw), rest)
          }
        }
      } else Failure("Not arbitrary block data", in)
    } catch {
      case e: ClassCastException => Failure("block data can only appear in a byte-based input", in)
      case e: NumberFormatException => Failure(e.getMessage, in)
    }
  }
  
  def blockDataBytes: Parser[ByteSeq] = new Parser[ByteSeq] {
    def apply(in: Input) = parseBlockData(in) match {
      case Success((data,all), rest) =>
        Success(data,rest)
      case f: NoSuccess => f
    }
  }

  def blockData: Parser[ByteCharSeq] = new Parser[ByteCharSeq] {
    def apply(in: Input) = parseBlockData(in) match {
      case Success((data,raw), rest) =>
        Success(raw,rest)
      case f: NoSuccess => f
    }
  }
  
  def nr1: Parser[ByteCharSeq] = nr1Expr
  def nr2: Parser[ByteCharSeq] = nr2Expr
  def nr3: Parser[ByteCharSeq] = nr3Expr
  def nrf: Parser[ByteCharSeq] = nr3 | nr2 | nr1
  def dqString: Parser[ByteCharSeq] = dqStringExpr
  def sqString: Parser[ByteCharSeq] = sqStringExpr
  def string: Parser[ByteCharSeq] = dqString | sqString
  def chars: Parser[ByteCharSeq] = recMnemonicExpr

  def value: Parser[ByteCharSeq] = skipWS (
    blockData |
    nrf | 
    nr1 |
    string |
    chars
  )

  //!! missing: Suffix Program Data: ((ws?)~multiplier?)~unit

  // Parses a Response Message Unit
  def rmu: Parser[Result] = regularRMU | aardRMU

  // Parses a regular (non-AARD) RMU
  def regularRMU: Parser[Result] =
    skipWS(rep1sep(value, ",")) <~ skipWS(";" | EOI) ^^ { values => Result(values : _*) }

  // Parses an RMU with Arbitrary ASCII Response Data (e.g. *IDN? responses)
  def aardRMU: Parser[Result] =
    aardExpr <~ EOI ^^ { bytes => Result(bytes) }
  
  def response: Parser[Response] =
    nonEmptyResponse | emptyResponse

  def emptyResponse: Parser[Response] =
    skipWS(EOI) ^^ { _ => Response(Result()) }

  def nonEmptyResponse: Parser[Response] =
    skipWS(rep1(rmu) <~ EOI ^^ { results => Response(results : _*) })

  def recMnemonic: Parser[RecMnemonic] =
    recMnemonicExpr ^^ { mnem => RecMnemonic(mnem) }
  
  def header = ccqHeader | icHeader

  def ccqHeader: Parser[CCQHeader] =
    "*" ~> recMnemonic ^^ { mnem => CCQHeader(ByteCharSeq(mnem.getBytes: _*).toString) }
    
  def mnemSuffix: Parser[Int] = mnemSuffixExpr ^^ { _.toString.toInt }

  def icHeaderPart: Parser[ICHeaderPart] =
    recMnemonic ~ (mnemSuffix?) ^^
      { case mnem ~ suffix => ICHeaderPart(mnem, suffix getOrElse 1) }

  def icHeader = icHeaderRel | icHeaderAbs
  
  def icHeaderRel: Parser[ICHeaderRel] =
    rep1sep(icHeaderPart, ":") ^^ { parts => ICHeaderRel(parts : _*) }
  
  def icHeaderAbs: Parser[ICHeaderAbs] =
    ":"~>icHeaderRel ^^ { rel => ICHeaderAbs(rel.parts : _*) }

  def instruction = query | command
  
  def query: Parser[Query] =
    skipWS((header<~"?") ~ (ws ~ repsep(value, ",")?)) ^^ {
      case header ~ Some(ws ~ params) => Query(header, params :_*)
      case header ~ None => Query(header)
    }

  def command: Parser[Command] =
    skipWS(header ~ (ws ~ repsep(value, ",")?)) ^^ {
      case header ~ Some(ws ~ params) => Command(header, params :_*)
      case header ~ None => Command(header)
    }

  def request: Parser[Request] =
    skipWS(repsep(instruction, ";") ^^ { instr => Request(instr : _*) })
    
  /** Extract a CR+LF or LF terminated message from a CharSequence */
  def extractTermMsg(in: ByteSeq) =
    streamMsgRaw(ByteCharSeqReader(in))

  /** Extract a CR+LF or LF terminated message from a Reader */
  def extractTermMsg(in: Input) =  streamMsgRaw(in)

  def parseResponse(in: ByteSeq): Response =
    parseAll(response, ByteCharSeqReader(in)).get

  def parseHeader(in: ByteSeq): Header =
    parseAll(header, ByteCharSeqReader(in)).get

  def parseRequest(in: ByteSeq): Request =
    parseAll(request, ByteCharSeqReader(in)).get
}


object SCPIParser {
  val streamMsgTermExpr = """(\x0D?\x0A)""".r
  val nonBlockStringExpr = """(([^#'"\x0D\x0A]|[#][^0-9])+)""".r
  val nr1Expr = """-?\d+""".r
  val nr2Expr = """(\d+(\.\d*)?|\d*\.\d+)""".r
  val nr3Expr = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
  val aardExpr = """(\p{ASCII}+)""".r
  val dqStringExpr = """"([^"]*)"""".r
  val sqStringExpr = """'([^']*)'""".r
  val mnemSuffixExpr = """[1-9][0-9]*""".r
  val recMnemonicExpr = """[A-Z]+|[a-z]+""".r
  val EOI = """\z""".r
  
  protected val tlParser = new ThreadLocal[SCPIParser]

  def parser = {
    if (tlParser.get == null) tlParser.set(new SCPIParser)
    tlParser.get
  }

  def apply() = parser
}
