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


sealed trait SetVal {
  def value: Any
  def toDouble: Double
  def toBoolean: Boolean
  def toNative: Any
  def toJsValue: JsValue

  def toSeq: Seq[SetVal] = value.asInstanceOf[Seq[SetVal]]
  def toSettings: Settings = value.asInstanceOf[Settings]
  def toMap: Map[String, SetVal] = toSettings.value

  def toByte: Byte = toDouble.toByte
  def toChar: Char = toDouble.toChar
  def toShort: Short = toDouble.toShort
  def toInt: Int = toDouble.toInt
  def toLong: Long = toDouble.toLong
  def toFloat: Float = toDouble.toFloat

  override def toString = toJsValue.toString
  def toJSON = toString
}


object SetVal {
  def apply(value: Any): SetVal = value match {
    case x: SetVal => x
    case x: Boolean => BoolSetVal(x)
    case x: Byte => NumSetVal(x)
    case x: Char => NumSetVal(x)
    case x: Short => NumSetVal(x)
    case x: Int => NumSetVal(x)
    case x: Long => NumSetVal(x)
    case x: Float => NumSetVal(x)
    case x: Double => NumSetVal(x)
    case x: Unit => throw new ClassCastException("Unit cannot be cast to " + classOf[SetVal].getName)
    case x: Map[_, _] => Settings.fromNative(x)
    case x: Seq[_] => SeqSetVal.fromNative(x)
    case x: AnyRef => throw new ClassCastException(x.getClass.getName + " cannot be cast to " + classOf[SetVal].getName)
  }

  def apply(value: Seq[SetVal]): SetVal = SeqSetVal(value: _*)

  def apply(value: Map[String, SetVal]): SetVal = Settings(value)
  
  def fromJsValue(value: JsValue): SetVal = value match {
    case JsNull => null
    case x: JsBoolean => BoolSetVal(x match { case JsTrue => true; case JsFalse => false })
    case x: JsNumber => NumSetVal(x.self.toDouble)
    case x: JsString => StringSetVal(x.self)
    case x: JsArray => SeqSetVal((x.self map {v => SetVal.fromJsValue(v)}): _*)
    case x: JsObject => Settings.fromJsObject(x)
  }

  def fromJSON(json: String): SetVal = fromJsValue(Js(json))
}



case class NumSetVal(value: Double) extends SetVal {
  def toDouble = value
  def toBoolean = (value >= 0)
  def toNative: Double = value
  def toJsValue: JsNumber = {
    val longVal = value.toLong
    if (value != longVal) JsNumber(value) else JsNumber(longVal)
  }
}


case class BoolSetVal(value: Boolean) extends SetVal {
  def toDouble = if (value) 1. else 0.
  def toBoolean = value
  def toNative: Boolean = value
  def toJsValue: JsBoolean = if (value) JsTrue else JsFalse
}


case class StringSetVal(value: String) extends SetVal {
  def toDouble = value.toDouble
  def toBoolean = value.toBoolean
  def toNative: String = value
  def toJsValue: JsString = JsString(value)
}



trait ComplexSetVal extends SetVal {
  def toDouble = throw new ClassCastException(this.getClass.getName + " cannot be cast to Double")
  def toBoolean = throw new ClassCastException(this.getClass.getName + " cannot be cast to Boolean")
}



case class SeqSetVal(value: SetVal*) extends ComplexSetVal {
  def toNative: Seq[Any] = value map {_.toNative}
  def toJsValue = JsArray(value.toList map {_ toJsValue})
}

object SeqSetVal {
  def fromNative(x: Seq[Any]): SeqSetVal = SeqSetVal((x map {v => SetVal(v)}) :_*)
}



case class Settings(value: Map[String, SetVal]) extends ComplexSetVal {
  def apply(key: String): SetVal = value(key)
  def apply(key: Symbol): SetVal = value(key.name)
  def apply(key: Int): SetVal = value(key.toString)
  
  def merge(that: Settings): Settings = {
    Settings(
      that.value.foldLeft(this.value) { case (result, (k, vNew)) =>
        result.get(k) match {
          case Some(vOld) => (vOld, vNew) match {
            case (vOld: Settings, vNew: Settings) =>
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


object Settings {
  val empty = Settings(Map.empty[String, SetVal])

  def KeyString(k: Any) = k match {
    case k: String => k
    case k: Symbol => k.name
    case k: Int => k.toString
    case k: Any => k.toString
  }

  def apply(values: (_, _)*): Settings = {
    values.foldLeft(Settings.empty) { case (settings, (path, value)) =>
      settings merge {
        val pathComponents = path.toString.split("[.]")
        pathComponents.reverse.foldLeft(SetVal(value)) {
          case (v, k) => Settings(Map(k -> v))
        }.asInstanceOf[Settings]
      }
    }
  }

  def fromNative(values: Map[_, _]) =
    Settings(values map { case (k, v) => (KeyString(k), SetVal(v)) })
  
  def fromJsObject(obj: JsObject) =
    Settings( obj.self.map{ case (k,v) => (k.self, SetVal.fromJsValue(v)) } )
  
  def fromJSON(json: String) = SetVal.fromJSON(json).asInstanceOf[Settings]
}
