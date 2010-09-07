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

case class RingBuffer[A: ClassManifest](val length: Int, initWith: A) extends Mutable with Seq[A] {
  ringBuffer =>

  protected var pos = 0
  protected val content = Array.fill(size)(initWith)
  protected def realIdx(i: Int) = 
    if ((pos < 0) || (pos >= length)) throw new java.lang.IndexOutOfBoundsException
    else (pos + i) % size

  def apply(i: Int) = content(realIdx(i))

  def update(i: Int, x: A) = content(realIdx(i)) = x

  def pushBack(x: A) = {
    pos = (pos + 1) % size
    content((pos + size - 1) % size) = x
  }

  def pushFront(x: A) = {
    pos = (pos + size - 1) % size
    content(pos) = x
  }
  
  def iterator = new Iterator[A] {
    var pos = 0

    def hasNext = pos < (ringBuffer.size)
    
    def next = {
      val x = ringBuffer(pos)
      pos = pos + 1
      x
    }
  }
}
