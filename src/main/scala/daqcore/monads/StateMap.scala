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


package daqcore.monads


case class StateMap(states: Map[Any, Any] = Map.empty[Any, Any]) {
  def apply[T](key: Any): T = states.get(key) match {
    case Some(value) =>
      try { value.asInstanceOf[T] }
      catch { case e: ClassCastException => throw new RuntimeException("StateMap: Type mismatch", e) }
    case None => throw new RuntimeException("StateMap: No such key: " + key.toString)
  }
  
  def get[T](key: Any): Option[T] = states.get(key) map { value =>
    try { value.asInstanceOf[T] }
    catch { case e: ClassCastException => throw new RuntimeException("StateMap: Type mismatch") }      
  }

  def set[T](key: Any)(value: => Any) =
    StateMap(states + (key -> value))
  
  def modify[T](key: Any)(f: T => Any) = set(key)(f(apply[T](key)))

  def modify[T](key: Any, default: => T)(f: T => Any) =
    set(key) { get[T](key) map f getOrElse f(default) }
}
