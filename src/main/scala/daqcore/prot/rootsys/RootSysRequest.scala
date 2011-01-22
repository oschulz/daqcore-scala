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


package daqcore.prot.rootsys

import daqcore.util._
import daqcore.actors._


// Not thread-safe:
case class ContentSerCache() {
  protected var map = Map.empty[ClassManifest[_], ContentSerializer[_]]
  
  def forType[A : ClassManifest]: ContentSerializer[A] = {
    val mf = classManifest[A]
    map.get(mf) match {
      case Some(serializer) => serializer.asInstanceOf[ContentSerializer[A]]
      case None => {
        val serializer = ContentSerializer.forType[A](mf)
        map = map + (mf -> serializer)
        serializer
      }
    }
  }
}

sealed trait RootSysRequest {
  val requestName = getClass.shortName
  val requestMf = scala.reflect.ClassManifest.fromClass(getClass).asInstanceOf[ClassManifest[RootSysRequest]]
  
  def writeRequest(out: BasicOutput)(implicit serializerCache: ContentSerCache): Unit = {
    out.writeString(requestName)
    serializerCache.forType[RootSysRequest](requestMf).write(out, this)
  }
}


abstract class RootSysQuery[R: ClassManifest] extends ActorQuery[R] with RootSysRequest {
  def readResponse(in: BasicInput)(implicit serializerCache: ContentSerCache): R = {
    serializerCache.forType[R](replyMF).read(in)
  }
}

abstract class RootSysCmd extends ActorCmd with RootSysRequest
