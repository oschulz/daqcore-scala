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

  trait ParseFunction extends ((Array[Byte], Int) => ParseReturn)
    { def apply(input: Array[Byte], pos: Int): ParseReturn }
  
  implicit def parseFunction(f: (Array[Byte], Int) => ParseReturn) =
    new ParseFunction { def apply(input: Array[Byte], pos: Int) = f(input, pos) }
  
  protected def parseStart(input: Array[Byte], pos: Int): ParseReturn
  
  case class SubArray(array: Array[Byte], from: Int, until: Int) {
    require ((from >= 0) && (until >= from))
    def size = until - from
    def toArray = array.slice(from, until)
  }

  protected var inputQueue: List[SubArray] = Nil
  protected var parseFunc = parseStart _
  
  def unfinished: Boolean = ! inputQueue.isEmpty

  def apply(bytes: Seq[Byte]): Seq[Seq[Byte]] = {
    var extracted: List[Seq[Byte]] = Nil
  
    trace("apply(%s)".format(loggable(ByteCharSeq(bytes: _*))))

    val array = bytes.toArray
    var startPos = 0
    var pos = 0
    while (pos < array.size) {
      val (msgComplete, nextPos, nextParseFunc) = parseFunc(array, pos)
      pos = nextPos
      parseFunc = nextParseFunc
      if (msgComplete) {
        inputQueue = SubArray(array, startPos, pos) :: inputQueue
        val totalSize = inputQueue.view map {_.size} sum
        val outputArray = Array.ofDim[Byte](totalSize)
        var outputPos = 0
        for (in <- inputQueue.reverse) {
          for (i <- 0 to in.size - 1) outputArray(outputPos + i) = in.array(in.from + i)
          outputPos += in.size
        }
        val msg = outputArray.toIISeq
        trace("Complete sequence of length %s available: [%s]".format(msg.length, loggable(msg)))
        extracted = msg :: extracted
        startPos = pos
        inputQueue = Nil
      }
    }
    if (startPos < pos) inputQueue = SubArray(array, startPos, pos) :: inputQueue
    // trace("inputQueue:" + loggable(inputQueue map loggable))
    
    extracted.reverse
  }
}
