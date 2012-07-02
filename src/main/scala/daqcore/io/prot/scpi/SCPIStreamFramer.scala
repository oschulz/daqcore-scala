// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


import daqcore.io._
import daqcore.util._


object SCPIStreamFramer extends Codec[ByteString, ByteString] {
  def charEncoding = "ASCII"

  val DQUOTE = ByteString("\"")
  val SQUOTE = ByteString("'")
  val CR = ByteString("\r")
  val NL = ByteString("\n")
  val CRNL = (CR ++ NL).compact
  val HASH = ByteString("#")


  private def encFct(out: ByteStringBuilder, msg: ByteString): Unit = {
    if (msg endsWith CRNL) out ++= msg
    else if (msg endsWith NL) { out ++= msg.dropRight(1) ++= CRNL }
    else { out ++= msg ++= CRNL }
  }

  val enc = encFct(_, _)
  

  def decodeBlockData = IO take 1 flatMap {
    lenA =>
    val lengthSize = lenA.decodeString(charEncoding).toInt
    if (lengthSize == 0) IO throwErr(new UnsupportedOperationException("Indefinite-Length Block Data not supported"))
    else for {
      lenB <- IO take lengthSize
      length = lenB.decodeString(charEncoding).toInt
      data <- IO take length
    } yield {
      if (data.length != length) IO throwErr(new RuntimeException("EOI while expecting more block data"))
      (lenA, lenB, data)
    }
  }

  
  def decNext(prev: ByteString): IO.Iteratee[ByteString] = IO take 1 flatMap {
    current =>

    def notSpecial(byte: Byte) = (byte != DQUOTE.head) && (byte != SQUOTE.head) &&
      (byte != CR.head) && (byte != NL.head) && (byte != HASH.head)

    def continue(next: ByteString) = decNext(prev ++ current ++ next)

    def takeBlockData = decodeBlockData map { case (lenA, lenB, data) => lenA ++ lenB ++ data }
    
    current match {
      case DQUOTE => IO takeUntil(DQUOTE, true) flatMap continue
      case SQUOTE => IO takeUntil(SQUOTE, true) flatMap continue
      case CR =>
        IO take 1 flatMap {
          case NL => IO Done prev ++ current ++ NL
          case _ => IO throwErr(new RuntimeException("Expected NL after CR"))
        }
      case NL => IO Done prev ++ current
      case HASH => takeBlockData flatMap continue
      case _ => IO takeWhile notSpecial flatMap continue
    }
  }
  
  val dec = decNext(ByteString.empty)
}
