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

import java.lang.reflect.Method
import java.lang.annotation.{Annotation => JAnnotation}


class MethodOps(m: Method) {
  def findSameIn[A](that: Class[A]): Option[Method] = {
    try { Some(that.getMethod(m.getName, m.getParameterTypes: _*)) }
    catch { case e: NoSuchMethodException => None }
  }

  def findAnnotation[A <: JAnnotation : ClassManifest]: Option[A] = {
    val annotClass: Class[JAnnotation] = classManifest[A].erasure.asInstanceOf[Class[JAnnotation]]
    
    m.getAnnotation(annotClass) match {
      case null => (
        for {
          s <- m.getDeclaringClass.getSuperclass match {
            case null => None
            case scl => findSameIn(scl)
          }
          a <- new MethodOps(s).findAnnotation[A]
        } yield a
      ) match {
        case r @ Some(a) => r
        case None => {
          val ifaceAnnots = for {
            iface <- m.getDeclaringClass.getInterfaces.toStream
            ifaceMethod <- findSameIn(iface)
            ifaceAnnot <- new MethodOps(ifaceMethod).findAnnotation[A]
          } yield { ifaceAnnot }
          if (ifaceAnnots.isEmpty) None else Some(ifaceAnnots.head)
        }
      }
      case annot => Some(annot.asInstanceOf[A])
    }
  }
}
