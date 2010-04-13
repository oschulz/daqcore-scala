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


class ProfileInfo(val symbol: Symbol) {
  override def toString() = symbol.name

  override def hashCode = symbol.hashCode

  def canEqual(that: Any) = that.isInstanceOf[ProfileInfo]

  override def equals(that: Any) = canEqual(that) && (
    that match {
      case ProfileInfo(thatSymbol) => symbol == thatSymbol
      case _ => false
    }
  )
}


object ProfileInfo {
  def unapply(profile: ProfileInfo) = Some(profile.symbol)

  def apply[T <: Profile : ClassManifest]: ProfileInfo =
    new ProfileInfo(Symbol(classManifest[T].toString))
  
  def isProfile(c:Class[_]) : Boolean = {
    val profClass = classOf[Profile]
    profClass.isAssignableFrom(c) &&
    c != profClass
  }

  def apply(c:Class[_]): ProfileInfo = {
    require(isProfile(c))
    new ProfileInfo(Symbol(c.getName))
  }

  def profilesOf(cl: Class[_]) : Set[ProfileInfo] = {
    val profClass = classOf[Profile]
    val interfaces = cl.getGenericInterfaces.toSet
    
    interfaces flatMap { interface =>
      interface match {
        case c:Class[_] if isProfile(c) =>
          Set(ProfileInfo(c)) ++ profilesOf(c)
        case _ => Set.empty[ProfileInfo]
      }
    }
  }
}
