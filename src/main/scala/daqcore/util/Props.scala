// Copyright (C) 2011 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import dispatch.json._


case class PropPath(parts: collection.immutable.Queue[String]) {
  def isEmpty: Boolean = parts.isEmpty
  def head: String = parts.head
  def tail: PropPath = PropPath(parts.tail)

  def %(k: Any): PropPath = k match {
    case k: String => PropPath(parts ++ k.split("[.]").toSeq)
    case k: Symbol => PropPath(parts enqueue k.name)
    case k: Int => PropPath(parts enqueue k.toString)
    case k: Seq[_] => PropPath(parts ++ (k map {_.toString}))
    case k: Any => %(k.toString)
  }

  override def toString = parts.mkString(".")
}


object PropPath {
  val empty: PropPath = PropPath(collection.immutable.Queue.empty[String])

  def apply(): PropPath = empty

  def apply(k: Any): PropPath = k match {
    case k: PropPath => k
    case _ => empty % k
  }
}



sealed trait PropVal {
  def value: Any
  def toDouble: Double
  def toBoolean: Boolean
  def toNative: Any
  def toJsValue: JsValue

  def toSeq: Seq[PropVal] = value.asInstanceOf[Seq[PropVal]]
  def toSettings: Props = value.asInstanceOf[Props]
  def toMap: Map[String, PropVal] = toSettings.value

  def toByte: Byte = toDouble.toByte
  def toChar: Char = toDouble.toChar
  def toShort: Short = toDouble.toShort
  def toInt: Int = toDouble.toInt
  def toLong: Long = toDouble.toLong
  def toFloat: Float = toDouble.toFloat

  def toProps = this.asInstanceOf[Props]
  
  def apply(path: PropPath): PropVal = throw new UnsupportedOperationException("PropVals of type " + this.getClass.getName + " have no properties")
  def apply(path: String): PropVal = apply(PropPath(path))
  def apply(key: Int): PropVal = apply(key.toString)
  
  override def toString = toJsValue.toString
  def toJSON = toString
}


object PropVal {
  def apply(value: Any): PropVal = value match {
    case x: PropVal => x
    case x: Boolean => BoolPropVal(x)
    case x: Byte => NumPropVal(x)
    case x: Char => NumPropVal(x)
    case x: Short => NumPropVal(x)
    case x: Int => NumPropVal(x)
    case x: Long => NumPropVal(x)
    case x: Float => NumPropVal(x)
    case x: Double => NumPropVal(x)
    case x: String => StringPropVal(x)
    case x: Unit => throw new IllegalArgumentException(classOf[PropVal].getName + " cannot represent " + classOf[Unit].getName)
    case x: Map[_, _] => Props.fromNative(x)
    case x: Seq[_] => SeqPropVal.fromNative(x)
    case x: AnyRef => throw new IllegalArgumentException(classOf[PropVal].getName + " cannot represent " + x.getClass.getName)
  }

  def apply(value: Seq[PropVal]): PropVal = SeqPropVal(value: _*)

  def apply(value: Map[String, PropVal]): PropVal = Props(value)
  
  def fromJsValue(value: JsValue): PropVal = value match {
    case JsNull => null
    case x: JsBoolean => BoolPropVal(x match { case JsTrue => true; case JsFalse => false })
    case x: JsNumber => NumPropVal(x.self.toDouble)
    case x: JsString => StringPropVal(x.self)
    case x: JsArray => SeqPropVal((x.self map {v => PropVal.fromJsValue(v)}): _*)
    case x: JsObject => Props.fromJsObject(x)
  }

  def fromJSON(json: String): PropVal = fromJsValue(Js(json))
}



case class NumPropVal(value: Double) extends PropVal {
  def toDouble = value
  def toBoolean = (value >= 0)
  def toNative: Double = value
  def toJsValue: JsNumber = {
    val longVal = value.toLong
    if (value != longVal) JsNumber(value) else JsNumber(longVal)
  }
}


case class BoolPropVal(value: Boolean) extends PropVal {
  def toDouble = if (value) 1. else 0.
  def toBoolean = value
  def toNative: Boolean = value
  def toJsValue: JsBoolean = if (value) JsTrue else JsFalse
}


case class StringPropVal(value: String) extends PropVal {
  def toDouble = value.toDouble
  def toBoolean = value.toBoolean
  def toNative: String = value
  def toJsValue: JsString = JsString(value)
}



trait ComplexPropVal extends PropVal {
  def toDouble = throw new UnsupportedOperationException(this.getClass.getName + " cannot be converted to Double")
  def toBoolean = throw new UnsupportedOperationException(this.getClass.getName + " cannot be converted to Boolean")
}



case class SeqPropVal(value: PropVal*) extends ComplexPropVal {
  def toNative: Seq[Any] = value map {_.toNative}
  def toJsValue = JsArray(value.toList map {_ toJsValue})
}

object SeqPropVal {
  def fromNative(x: Seq[Any]): SeqPropVal = SeqPropVal((x map {v => PropVal(v)}) :_*)
}



case class Props(value: Map[String, PropVal]) extends ComplexPropVal {
  override def apply(path: PropPath): PropVal = {
    if (path.isEmpty) throw new IllegalArgumentException("Cannot get value for empty PropPath")
    value.get(path.head) match {
      case None => throw new IllegalArgumentException("Invalid PropPath " + path)
      case Some(v) => {
        if (path.tail.isEmpty) v
        else v match {
          case props: Props => props(path.tail)
          case _ => throw new IllegalArgumentException("Invalid PropPath " + path.tail)
        }
      }
    }
  }

  override def apply(key: Int): PropVal = value(key.toString)
    
  def merge(that: Props): Props = {
    Props(
      that.value.foldLeft(this.value) { case (result, (k, vNew)) =>
        result.get(k) match {
          case Some(vOld) => (vOld, vNew) match {
            case (vOld: Props, vNew: Props) =>
              result + {val e = (k, (vOld merge vNew)); e}
            case (vOld, vNew) =>
              if (vOld.getClass == vNew.getClass) result + { val e = (k, vNew); e }
              else throw new IllegalArgumentException("Can't merge setting of class %s with class %s".format(vOld.getClass.getName, vNew.getClass.getName))
          }
          case None => result + { val e = (k, vNew); e }
        }
      }
    )
  }

  def toNative: Map[String, Any] = value map {case (k,v) => (k, v.toNative)}
  def toJsValue = JsObject(value.toList map {case (k,v) => (JsString(k), v.toJsValue)})
}


object Props {
  val empty = Props(Map.empty[String, PropVal])

  def apply(values: (_, _)*): Props = {
    values.foldLeft(Props.empty) { case (settings, (path, value)) =>
      settings merge {
        PropPath(path).parts.reverse.foldLeft(PropVal(value)) {
          case (v, k) => Props(Map(k -> v))
        }.asInstanceOf[Props]
      }
    }
  }

  def fromNative(values: Map[_, _]) =
    Props((values.toSeq map { case (k, v) => (k, PropVal(v)) }) : _*)
  
  def fromJsObject(obj: JsObject) =
    Props( obj.self.map{ case (k,v) => (k.self, PropVal.fromJsValue(v)) } )
  
  def fromJSON(json: String) = PropVal.fromJSON(json).asInstanceOf[Props]
}
