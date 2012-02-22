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


package daqcore.io

import daqcore.util._


case class Codec[A, B] (enc: Encoder[A], dec: Decoder[B])


case class ListDecoder[A](dec: Decoder[A]) {
  protected val buffer = collection.mutable.ListBuffer[A]()
  protected val procInput = IO.IterateeRef sync { IO repeat {
    for { elem <- dec } yield { buffer += elem }
  } }
  
  def apply(input: IO.Input): List[A] = { procInput(input); val res = buffer.result; buffer.clear; res }
  def apply(input: ByteString): List[A] = apply(IO Chunk input)
}


object LineCodec {
  val NL = ByteString("\n")

  def enc[A]: Encoder[A] = (in: A, out: ByteStringBuilder) => {
	val bs = in match {
      case bs: ByteString => bs
	  case obj => ByteString(obj.toString)
	}
	out ++= bs
	if (! bs.endsWith(NL)) out ++= NL
  }
  
  def dec: Decoder[ByteString] = IO takeUntil ByteString("\n")
  def decString: Decoder[String] = dec map { _.utf8String }
}
