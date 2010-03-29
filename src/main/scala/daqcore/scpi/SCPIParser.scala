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


object SCPIParser extends JavaTokenParsers with PackratParsers with Logging {
  lazy val blockData: Parser[IndexedSeq[Byte]] = new Parser[IndexedSeq[Byte]] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      def failure(msg: String) = Failure(msg, in.drop(start - offset))
      try {
        val input = in.source.asInstanceOf[ByteCSeq].contents.subSequence(start, source.length)
        if ( (input.length >= 0) && (input(0) == '#') && (input(1) >= '0') && (input(1) <= '9')) {
          val sizeLength = input(1).toChar.toString.toInt
          val dataOffset = sizeLength + 2
          if ((sizeLength <=  0) || (sizeLength > 10)) failure("Invalid block data size length: " + sizeLength)
          else if (input.length < dataOffset) failure("Block data input to short for size length " + sizeLength)
          else {
            val size = input.subSequence(2, dataOffset).map(_.toChar).mkString.toInt
            val dataEnd = dataOffset + size
            if (input.size < dataEnd) failure("Block data input to short for size " + size)
            val data = input.subSequence(dataOffset, dataEnd)
            val rest = in.drop(start - offset + dataEnd)
            require(data.length == size)
            Success(data, rest)
          }
        } else failure("Not arbitrary block data")
      } catch {
        case e: ClassCastException => failure("block data can only appear in a byte-based input")
        case e: NumberFormatException => failure(e.getMessage)
      }
    }
  }

  lazy val nr1: PackratParser[String] = wholeNumber
  lazy val nrf: PackratParser[String] = floatingPointNumber
  lazy val string: PackratParser[String] = stringLiteral
  
  lazy val value: PackratParser[Any] = 
    blockData |
    nrf | 
    nr1 |
    string

  lazy val params: PackratParser[List[Any]] =
    repsep(value, ",")
  
  lazy val respMsg: PackratParser[List[List[Any]]] =
    repsep(params, ";")
}
