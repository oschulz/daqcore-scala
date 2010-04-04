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


package daqcore.util


import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.matching.Regex


class ByteCSeqParsers extends RegexParsers with PackratParsers with Logging {
  override def skipWhitespace = false

  implicit def expr(ex: Regex) = new PackratParser[ByteCSeq] {
    def apply(in: Input) = {
      val (source, offset) = (in.source, in.offset)
      (ex.findPrefixMatchOf(source.subSequence(offset, source.length))) match {
        case Some(m) =>  Success(ByteCSeq(source.subSequence(offset, offset + m.end)), in.drop(m.end))
        case None => Failure("Input \"%s\" does not match expression \"%s\"".format(in.first, ex), in)
      }
    }
  }


  def ws: Parser[ByteCSeq] = expr(whiteSpace)
  
  def skipWS[T](parser: Parser[T]): Parser[T] = (ws?) ~> parser <~ (ws?)
}