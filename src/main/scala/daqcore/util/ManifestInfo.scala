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


case class ManifestInfo(mf: ClassManifest[_]) {
  import java.lang.Class
  import java.lang.reflect.{Type => JType, Field, ParameterizedType, GenericArrayType, WildcardType, TypeVariable }


  val genTypeMap: Map[TypeVariable[_], ClassManifest[_]] = {
    val typeArgs = mf.erasure.getTypeParameters map {_.asInstanceOf[TypeVariable[_]]}
    val typeArgMfs = mf.typeArguments map {_.asInstanceOf[ClassManifest[_]]}

    Map(typeArgs zip typeArgMfs :_*)
  }
  

  def manifestOf(tp: JType): ClassManifest[_] = {
    import scala.reflect.ClassManifest.{classType, arrayType, fromClass}
    import scala.reflect.ClassManifest

    def fromTypeArg(tp: JType): ClassManifest[_] = tp match {
      case cl: Class[_] => {
        if (cl == classOf[java.lang.Byte]) classManifest[Byte]
        else if (cl == classOf[java.lang.Short]) classManifest[Short]
        else if (cl == classOf[java.lang.Character]) classManifest[Char]
        else if (cl == classOf[java.lang.Integer]) classManifest[Int]
        else if (cl == classOf[java.lang.Long]) classManifest[Long]
        else if (cl == classOf[java.lang.Float]) classManifest[Float]
        else if (cl == classOf[java.lang.Double]) classManifest[Double]
        else if (cl == classOf[java.lang.Boolean]) classManifest[Boolean]
        else if (cl == classOf[java.lang.Void]) classManifest[Unit]
        else manifestOf(tp)
      }
      case _ => manifestOf(tp)
    }

    tp match {
      case x: Class[_] => fromClass(x)
      case x: ParameterizedType => {
        val owner = x.getOwnerType
        val raw = x.getRawType() match { case clazz: Class[_] => clazz }
        val targs = x.getActualTypeArguments() map fromTypeArg

        if (owner != null) classType(manifestOf(owner), raw, targs: _*)
        else {
          if (!targs.isEmpty) classType(raw, targs.head, targs.tail: _*)
          else manifestOf(raw)
        }
      }
      case x: GenericArrayType => arrayType(manifestOf(x.getGenericComponentType))
      case x: TypeVariable[_] => genTypeMap(x)
      case x: WildcardType => throw new IllegalArgumentException("ManifestInfo.manifestOf does not support WildcardType")
        // wildcardType(intersectionType(x.getLowerBounds map manifestOf: _*), intersectionType(x.getUpperBounds map manifestOf: _*))
    }
  }
}
