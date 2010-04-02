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


import scala.util.parsing.combinator._
import daqcore.util._


class SCPIParser extends JavaTokenParsers with PackratParsers with Logging {
  override def skipWhitespace = false

  def ws: Parser[String] = regex(whiteSpace)
  
  def skipWS[T](parser: Parser[T]): Parser[T] = (ws?) ~> parser <~ (ws?)
  
  def parseBlockData(in: Input): ParseResult[(IndexedSeq[Byte], ByteCSeq)] = {
    val source = in.source
    val offset = in.offset
    try {
      val inSeq = in.source.asInstanceOf[ByteCSeq]
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

  lazy val blockData: PackratParser[ByteCSeq] = new Parser[ByteCSeq] {
    def apply(in: Input) = parseBlockData(in) match {
      case Success((data,raw), rest) =>
        Success(raw,rest)
      case f: NoSuccess => f
    }
  }

  //!! Change implementation of these to avoid ByteCSeq -> String -> ByteCSeq conversion
  //!! Possibly one ASCII-type-argument parser can replace all of them
  lazy val nr1: PackratParser[ByteCSeq] = wholeNumber ^^ { v => ByteCSeq(v) }
  lazy val nrf: PackratParser[ByteCSeq] = floatingPointNumber ^^ { v => ByteCSeq(v) }
  lazy val string: PackratParser[ByteCSeq] = stringLiteral ^^ { v => ByteCSeq(v) }
  
  lazy val value: PackratParser[ByteCSeq] = skipWS (
    blockData |
    nrf | 
    nr1 |
    string
  )

  lazy val rmu: PackratParser[Result] =
    skipWS(repsep(value, ",") ^^ { values => Result(values : _*) })
  
  lazy val response: PackratParser[Response] =
    skipWS(repsep(rmu, ";") ^^ { results => Response(results : _*) })
  
  lazy val recMnemonic: PackratParser[RecMnemonic] =
    """[A-Z]+|[a-z]+""".r ^^ { mnem => RecMnemonic(ByteCSeq(mnem)) }
  
  lazy val header = ccqHeader | icHeader

  lazy val ccqHeader: PackratParser[CCQHeader] =
    "*" ~> recMnemonic ^^ { mnem => CCQHeader(mnem.charSeq.toString) }
    
  lazy val suffix: PackratParser[Int] = """[1-9][0-9]*""".r ^^ { _.toInt }

  lazy val icHeaderPart: PackratParser[ICHeaderPart] =
    recMnemonic ~ (suffix?) ^^
      { case mnem ~ suffix => ICHeaderPart(mnem, suffix getOrElse 1) }

  lazy val icHeader = icHeaderRel | icHeaderAbs
  
  lazy val icHeaderRel: PackratParser[ICHeaderRel] =
    repsep(icHeaderPart, ":") ^^ { parts => ICHeaderRel(parts : _*) }
  
  lazy val icHeaderAbs: PackratParser[ICHeaderAbs] =
    ":"~>icHeaderRel ^^ { rel => ICHeaderAbs(rel.parts : _*) }

  lazy val instruction = command | query
  
  lazy val command: PackratParser[Command] =
    skipWS(header ~ws~ repsep(value, ",")) ^^
      { case header ~ws~ params => Command(header, params :_*) }

  lazy val query: PackratParser[Query] =
    skipWS((header<~"?") ~ws~ repsep(value, ",")) ^^
      { case header ~ws~ params => Query(header, params :_*) }
  
  lazy val request: PackratParser[Request] =
    skipWS(repsep(instruction, ";") ^^ { instr => Request(instr : _*) })
  
  def parseResponse(in: java.lang.CharSequence): Response =
    parseAll(response, in).get

  def parseHeader(in: java.lang.CharSequence): Header =
    parseAll(header, in).get

  def parseRequest(in: java.lang.CharSequence): Request =
    parseAll(request, in).get
}


object SCPIParser {
  def apply() = new SCPIParser
}
