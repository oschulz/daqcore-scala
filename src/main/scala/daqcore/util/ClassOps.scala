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


class ClassOps(cl: Class[_]) {
  import java.lang.Class
  import java.lang.reflect.{Field, ParameterizedType}

  import ClassOps._

  def inheritance: List[Class[_]] = {
    def impl(cl: Class[_]): List[Class[_]] = cl :: (Option(cl.getSuperclass) map {impl(_)} getOrElse Nil)
    impl(cl)
  }
  
  def allFields: List[(Field, Manifest[_])] = {
    def fields = inheritance flatMap {_.getDeclaredFields}
    for { f <- fields } yield { f -> manifestOf(f.getGenericType) }
  }

  def wrapperClass: Class[_] = {
    if (cl.isPrimitive) wrapperClasses(cl)
    else cl
  }

  def shortName = {
    val shortClassNameExpr(_, name) = cl.getName
    name
  }
}


object ClassOps {
  import java.lang.reflect.{Type => JType, Array => _, _}
  import scala.reflect.Manifest.{classType, intersectionType, arrayType, wildcardType}
  import scala.reflect.Manifest

  val shortClassNameExpr = """(.*[$.])?([^$.]*)""".r

  def wrapperClasses = Map[Class[_], Class[_]] (
    classOf[Boolean] -> classOf[java.lang.Boolean],
    classOf[Byte] -> classOf[java.lang.Byte],
    classOf[Short] -> classOf[java.lang.Short],
    classOf[Int] -> classOf[java.lang.Integer],
    classOf[Long] -> classOf[java.lang.Long],
    classOf[Float] -> classOf[java.lang.Float],
    classOf[Double] -> classOf[java.lang.Double],
    classOf[Char] -> classOf[java.lang.Character],
    classOf[Unit] -> classOf[java.lang.Void]
  )

  protected def typeArgManifest(tp: JType): Manifest[_] = tp match {
    case cl: Class[_] => {
      if (cl == classOf[java.lang.Byte]) manifest[Byte]
      else if (cl == classOf[java.lang.Short]) manifest[Short]
      else if (cl == classOf[java.lang.Character]) manifest[Char]
      else if (cl == classOf[java.lang.Integer]) manifest[Int]
      else if (cl == classOf[java.lang.Long]) manifest[Long]
      else if (cl == classOf[java.lang.Float]) manifest[Float]
      else if (cl == classOf[java.lang.Double]) manifest[Double]
      else if (cl == classOf[java.lang.Boolean]) manifest[Boolean]
      else if (cl == classOf[java.lang.Void]) manifest[Unit]
      else manifestOf(tp)
    }
    case _ => manifestOf(tp)
  }

  def manifestFromClass(cl: Class[_]): Manifest[_] = cl match {
    case java.lang.Byte.TYPE => manifest[Byte]
    case java.lang.Short.TYPE => manifest[Short]
    case java.lang.Character.TYPE => manifest[Char]
    case java.lang.Integer.TYPE => manifest[Int]
    case java.lang.Long.TYPE => manifest[Long]
    case java.lang.Float.TYPE => manifest[Float]
    case java.lang.Double.TYPE => manifest[Double]
    case java.lang.Boolean.TYPE => manifest[Boolean]
    case java.lang.Void.TYPE => manifest[Unit]
    case _ => classType(cl)
  }
  
  // based on code by Paul Phillips
  protected def intersect(tps: JType*): Manifest[_] = intersectionType(tps map manifestOf: _*)

  // based on code by Paul Phillips
  def manifestOf(tp: JType): Manifest[_] = tp match {
    case x: Class[_]            => manifestFromClass(x)
    case x: ParameterizedType   => {
      val owner = x.getOwnerType
      val raw   = x.getRawType() match { case clazz: Class[_] => clazz }
      val targs = x.getActualTypeArguments() map typeArgManifest

      (owner == null, targs.isEmpty) match {
        case (true, true)   => manifestOf(raw)
        case (true, false)  => classType(raw, targs.head, targs.tail: _*)
        case (false, _)     => classType(manifestOf(owner), raw, targs: _*)
      }
    }
    case x: GenericArrayType => arrayType(manifestOf(x.getGenericComponentType))
    case x: WildcardType => wildcardType(intersect(x.getLowerBounds: _*), intersect(x.getUpperBounds: _*))
    case x: TypeVariable[_] => intersect(x.getBounds(): _*)
  }
}
