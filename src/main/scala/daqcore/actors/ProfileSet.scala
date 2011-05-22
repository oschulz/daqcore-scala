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


package daqcore.actors


case class ProfileSet(val classes: Set[Class[_]]) {
  def +(cl: Class[_]): ProfileSet = {
    require( ProfileSet.isProfile(cl) )
    new ProfileSet(
      if (classes exists { e => cl.isAssignableFrom(e) }) classes
      else (classes filter { e => ! e.isAssignableFrom(cl) }) ++ Set(cl)
    )
  }
  
  def +[A: ClassManifest]: ProfileSet = this.+(classManifest[A].erasure)
  def ++(that: ProfileSet) = that.classes.foldLeft(this) { _ + _ }
  
  def covers(cl: Class[_]): Boolean = classes exists { cl isAssignableFrom _ }
  
  override def toString = "ProfileSet(%s)".format(classes.mkString(", "))
}


object ProfileSet {
  protected val profClass = classOf[ServerProfile]

  val empty = ProfileSet()
  
  def isProfile(cl: Class[_]) : Boolean = {
    profClass.isAssignableFrom(cl)
  }

  def apply(classes: Class[_]*) = {
    classes foreach {c => require(isProfile(c))}
    new ProfileSet(Set(classes: _*))
  }
}
