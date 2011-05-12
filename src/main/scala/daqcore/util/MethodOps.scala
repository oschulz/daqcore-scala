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
  import MethodOps._

  def findSameIn[A](that: Class[A]): Option[Method] = {
    try { Some(that.getMethod(m.getName, m.getParameterTypes: _*)) }
    catch { case e: NoSuchMethodException => None }
  }

  def findAnnotation[A <: JAnnotation : ClassManifest]: Option[A] = {
    val cl = m.getDeclaringClass
    val annotClass: Class[JAnnotation] = classManifest[A].erasure.asInstanceOf[Class[JAnnotation]]

    def directAnnot = Option(m.getAnnotation(annotClass)) map {_.asInstanceOf[A]}

    def fieldSetterAnnot = if ((m.getName endsWith setterSuffix) && (m.getParameterTypes.length == 1)) {
      for {
        baseName <- Some(m.getName.dropRight(setterSuffix.length))
        field <- try { Some(cl.getDeclaredField(baseName)) } catch { case e: NoSuchFieldException => None }
        if (m.getParameterTypes()(0) == field.getType)
        getter <- try { Some(cl.getMethod(baseName)) } catch { case e: NoSuchMethodException => None }
        if (m.getParameterTypes()(0) == getter.getReturnType)
        annot <- Option(field.getAnnotation(annotClass)) map {_.asInstanceOf[A]}
      } yield { annot }
    } else None

    def fieldGetterAnnot = if (m.getParameterTypes.length == 0) {
      for {
        baseName <- Some(m.getName)
        field <- try { Some(cl.getDeclaredField(baseName)) } catch { case e: NoSuchFieldException => None }
        if (m.getReturnType == field.getType)
        annot <- Option(field.getAnnotation(annotClass)) map {_.asInstanceOf[A]}
      } yield { annot }
    } else None
    
    def superclassAnnot = for {
      s <- cl.getSuperclass match {
        case null => None
        case scl => findSameIn(scl)
      }
      a <- s.findAnnotation[A]
    } yield a
    
    def interfaceAnnot = {
      val ifaceAnnots = for {
        iface <- cl.getInterfaces.toStream
        ifaceMethod <- findSameIn(iface)
        ifaceAnnot <- ifaceMethod.findAnnotation[A]
      } yield { ifaceAnnot }
      if (ifaceAnnots.isEmpty) None else Some(ifaceAnnots.head)
    }
    
    directAnnot orElse fieldSetterAnnot orElse fieldGetterAnnot orElse superclassAnnot orElse interfaceAnnot
  }
}


object MethodOps {
    val setterSuffix = "_$eq"
}
