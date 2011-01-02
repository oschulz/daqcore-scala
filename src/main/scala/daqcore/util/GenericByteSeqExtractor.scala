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


trait GenericByteSeqExtractor extends Logging {
  type ParseReturn = (Boolean, Int, ParseFunction)

  trait ParseFunction extends ((ByteSeq, Int) => ParseReturn)
    { def apply(input: ByteSeq, pos: Int): ParseReturn }
  
  implicit def parseFunction(f: (ByteSeq, Int) => ParseReturn) =
    new ParseFunction { def apply(input: ByteSeq, pos: Int) = f(input, pos) }
  
  protected def parseStart(input: ByteSeq, pos: Int): ParseReturn
  
  case class BytesView(bytes: ByteSeq, from: Int, until: Int) {
    require ((from >= 0) && (until >= from))
    def size = until - from
    def iterator = bytes.iterator.slice(from, until)
    def toByteSeq = iterator.toSeq
  }

  protected var inputQueue: List[BytesView] = Nil
  protected var parseFunc = parseStart _
  
  def unfinished: Boolean = ! inputQueue.isEmpty

  def apply(bytes: ByteSeq): Seq[ByteSeq] = {
    var extracted: List[ByteSeq] = Nil
  
    trace("apply(%s)".format(loggable(ByteCharSeq(bytes: _*))))

    var startPos = 0
    var pos = 0
    while (pos < bytes.size) {
      val (msgComplete, nextPos, nextParseFunc) = parseFunc(bytes, pos)
      pos = nextPos
      parseFunc = nextParseFunc
      if (msgComplete) {
        inputQueue = BytesView(bytes, startPos, pos) :: inputQueue
        val totalSize = inputQueue.view map {_.size} sum
        val builder = ByteSeqBuilder()
        for (in <- inputQueue.reverse) builder ++= in.iterator
        val msg = builder.result()
        trace("Complete sequence of length %s available: [%s]".format(msg.length, loggable(msg)))
        extracted = msg :: extracted
        startPos = pos
        inputQueue = Nil
      }
    }
    if (startPos < pos) inputQueue = BytesView(bytes, startPos, pos) :: inputQueue
    // trace("inputQueue:" + loggable(inputQueue map loggable))
    
    extracted.reverse
  }
}
