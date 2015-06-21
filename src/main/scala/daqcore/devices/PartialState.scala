// Copyright (C) 2013-2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.devices

import scala.language.higherKinds
import scala.reflect.{ClassTag, classTag}

import daqcore.util._


sealed trait PartialState {
  def isEmpty: Boolean
}


object PartialState {
  def apply(): PartialState = Empty

  def empty: PartialState = Empty


  object Empty extends PartialState {
    def isEmpty = true
  }


  case class Single[+A](value: A) extends PartialState {
    def isEmpty = false
  }


  trait Multiple extends PartialState


  case class Indexed[+A](entries: ChV[PartialState]) extends Multiple {
    def isEmpty = entries.isEmpty
    def apply(key: Int): PartialState = entries(key)
    def get(key: Int): PartialState = entries.getOrElse(key, empty)
  }


  case class Group(entries: Map[PropKey, PartialState]) extends Multiple {
    def isEmpty = entries.isEmpty
    def apply(key: Symbol): PartialState = entries(key)
    def get(key: PropKey): PartialState = entries.getOrElse(key, Empty)
  }
}
