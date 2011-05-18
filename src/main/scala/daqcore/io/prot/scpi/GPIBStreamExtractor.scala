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
import daqcore.io.prot._


case class GPIBStreamExtractor() extends GenericByteSeqExtractor {
  protected def parseStart(input: ByteSeq, pos: Int): ParseReturn = {
    trace("parseStart(%s)".format(ByteCharSeq(input.slice(pos, input.size): _*)))
    val newline = '\n'.toByte
    val singleQuote = '"'.toByte
    val doubleQuote = '\''.toByte
    val hash = '#'.toByte
    
    var finished = false
    var p = pos
    var next: ParseFunction = null
    while ((p < input.size) && (next == null)) {
      input(p) match {
        case b @ (`singleQuote` | `doubleQuote`) => next = parseQuoted(b) _
        case b @ `newline` => { finished = true; next = parseStart _}
        case b @ `hash` => next = parseHash _ 
        case _ =>
      }
      p += 1
    }
    if (next == null) next = parseStart _
    (finished, p, next)
  }
  
  def parseHash(input: ByteSeq, pos: Int): ParseReturn = {
    trace("parseHash(%s)".format(ByteCharSeq(input.slice(pos, input.size): _*)))
    input(pos) match {
      case b if (b >= '1'.toByte) && (b <= '9'.toByte) =>
        (false, pos+1, parseDLBDSize(0, b.toByte.toChar.toString.toInt) _) // Definite Length Block Data
      case b if (b == '0'.toByte) => (false, pos + 1, parseStart _) // Arbitrary Length Block Data
      case _ => (false, pos + 1, parseStart _)
    }
  }
  
  def parseDLBDSize(acc: Int, remLen: Int)(input: ByteSeq, pos: Int): ParseReturn = {
    trace("parseDLBDSize(%s, %s)(%s)".format(acc, remLen, ByteCharSeq(input.slice(pos, input.size): _*)))
    var size = acc
    var r = remLen
    var p = pos
    while ((p < input.size) && (r > 0)) {
      size = 10 * size + input(p).toByte.toChar.toString.toInt
      p += 1; r -= 1
    }
    if (r > 0) (false, p, parseDLBDSize(size, r) _)
    else (false, p, parseDLBDContents(size) _)
  }
  
  def parseDLBDContents(remLen: Int)(input: ByteSeq, pos: Int): ParseReturn = {
    trace("parseDLBDContents(%s)(%s)".format(remLen, ByteCharSeq(input.slice(pos, input.size): _*)))
    if (pos + remLen > input.size) (false, input.size, parseDLBDContents(pos + remLen - input.size) _ )
    else (false, pos + remLen, parseStart _)
  }
  
  def parseQuoted(quoteChar: Byte)(input: ByteSeq, pos: Int): ParseReturn = {
    trace("parseQuoted(%s)(%s)".format(quoteChar, ByteCharSeq(input.slice(pos, input.size): _*)))
    var p = pos
    while ((p < input.size) && (input(p) != quoteChar)) p += 1
    if (p < input.size) (false, p+1, parseQuotedEsc(quoteChar) _)
    else (false, p, parseQuoted(quoteChar) _)
  }
  
  def parseQuotedEsc(quoteChar: Byte)(input: ByteSeq, pos: Int): ParseReturn = {
    trace("parseQuotedEsc(%s)(%s)".format(quoteChar, ByteCharSeq(input.slice(pos, input.size): _*)))
    if (input(pos) == quoteChar) (false, pos+1, parseQuoted(quoteChar) _)
    else (false, pos, parseStart _)
  }
}
