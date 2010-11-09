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


package daqcore.servers

import java.io.{File}

import daqcore.actors._
import daqcore.profiles._
import daqcore.util._


trait IntLenMsgInput extends InputFilterServer with RawMsgInput {
  override def source: ByteStreamInput
  val sourceCompanion = ByteStreamInput

  object extractor extends GenericByteSeqExtractor {
    protected def parseStart(input: Array[Byte], pos: Int): ParseReturn =
      (false, pos, parseMsgSize(0, 4) _)

    def parseMsgSize(acc: Int, remLen: Int)(input: Array[Byte], pos: Int): ParseReturn = {
      trace("parseMsgSize(%s, %s)(%s)".format(acc, remLen, ByteCharSeq(SubArray(input, pos, input.size).toArray)))
      var size = acc
      var r = remLen
      var p = pos
      while ((p < input.size) && (r > 0)) {
        size = (size << 8) + input(p)
        p += 1; r -= 1
      }
      if (r > 0) (false, p, parseMsgSize(size, r) _)
      else (false, p, parseMsgContents(size) _)
    }

    def parseMsgContents(remLen: Int)(input: Array[Byte], pos: Int): ParseReturn = {
      trace("parseMsgContents(%s)(%s)".format(remLen, ByteCharSeq(SubArray(input, pos, input.size).toArray)))
      if (pos + remLen > input.size) (false, input.size, parseMsgContents(pos + remLen - input.size) _ )
      else (true, pos + remLen, parseStart _)
    }
  }
  
  override def needMoreInput = extractor.unfinished
  
  protected def srvProcessInput(data: Seq[Byte]) = {
    trace("doHandleInput(%s)".format(loggable(data)))
    val extracted = extractor(data) map { _.toArray.drop(4).toIISeq }
    for (msg <- extracted) trace("Complete message of length %s available: [%s]".format(msg.length, loggable(ByteCharSeq(msg: _*))))
    extracted
  }
}


object IntLenMsgInput {
  def apply(stream: ByteStreamInput): IntLenMsgInput = {
    start(new IntLenMsgInput { val source = stream })
  }

  def apply(file: File, compression: Compression = Uncompressed): IntLenMsgInput = {
    start(new IntLenMsgInput { val source = InputStreamReader(file, compression) })
  }
}