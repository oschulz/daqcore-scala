// Copyright (C) 2011-2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import scala.language.implicitConversions

import scala.collection.{breakOut, GenTraversableOnce}
import scala.collection.immutable.SortedMap

import play.api.libs.json._
import org.apache.commons.codec.binary.Base64


sealed abstract class PropKey extends Ordered[PropKey] {
  def as[T](implicit conv: PropKey.To[T]) = conv.to(this)

  def asLong: Long = throw new UnsupportedOperationException(this.getClass.getName + " cannot interpreted as a Long")
  def asSymbol: Symbol = throw new UnsupportedOperationException("PropVals of type " + this.getClass.getName + " cannot be interpreted as a Symbol")

  def asInt: Int = asLong.toInt
  def asString: String = asSymbol.name

  def toPropVal: PropVal
  def toJsValue: JsValue
}


object PropKey {
  def apply[T](x: T)(implicit conv: From[T]) = conv.from(x)

  implicit def from[T](x: T)(implicit conv: From[T]): PropKey = PropKey(x)


  trait From[T] {
    def from(x: T): PropKey
  }

  trait To[T] {
    def to(propKey: PropKey): T
  }

  trait Converter[T] extends From[T] with To[T]


  implicit object PropKeyConverter extends Converter[PropKey] {
    def from(x: PropKey) = x
    def to(propKey: PropKey) = propKey
  }

  implicit object IntConverter extends Converter[Int] {
    def from(x: Int) = IntegerPropKey(x)
    def to(propKey: PropKey) = propKey.asInt
  }

  implicit object LongConverter extends Converter[Long] {
    def from(x: Long) = IntegerPropKey(x)
    def to(propKey: PropKey) = propKey.asLong
  }

  implicit object SymbolConverter extends Converter[Symbol] {
    def from(x: Symbol) = SymbolPropKey(x)
    def to(propKey: PropKey) = propKey.asSymbol
  }

  implicit object StringConverter extends Converter[String] {
    def from(x: String) = {
      try {
        IntegerPropKey(x.toLong)
      } catch {
        case err: NumberFormatException => SymbolPropKey(Symbol(x))
      }
    }

    def to(propKey: PropKey) = propKey.asString
  }
}


final case class IntegerPropKey (value: Long) extends PropKey {
  override def asLong = value
  def toPropVal = IntegerPropVal(value)
  def toJsValue = JsNumber(value)
  override def toString = value.toString

  def compare(that: PropKey): Int = that match {
    case other: IntegerPropKey => this.value compare other.value
    case other: SymbolPropKey => -1
  }
}


final case class SymbolPropKey (value: Symbol) extends PropKey {
  override def asSymbol = value
  def toPropVal = SymbolPropVal(value)
  def toJsValue = JsString(value.name)
  override def toString = value.name

  def compare(that: PropKey): Int = that match {
    case other: IntegerPropKey => +1
    case other: SymbolPropKey => this.value.hashCode compare other.value.hashCode
  }
}



case class PropPath(parts: collection.immutable.Queue[PropKey]) {
  def isEmpty: Boolean = parts.isEmpty
  def head: PropKey = parts.head
  def tail: PropPath = PropPath(parts.tail)

  def %(k: PropKey): PropPath = PropPath(parts enqueue k)

  def %(x: Int): PropPath = this % PropKey(x)

  def %(x: Long): PropPath = this % PropKey(x)

  def %(x: Symbol): PropPath = this % PropKey(x)

  def %(x: String): PropPath = {
    if (x contains '.') PropPath( parts ++ x.split("[.]").toSeq.map(s => PropKey(s)) )
    else this % PropKey(x)   
  }

  def %(that: PropPath): PropPath = PropPath(this.parts ++ that.parts)

  override def toString = parts.mkString(".")
}


object PropPath {
  def apply[T](x: T)(implicit conv: From[T]) = conv.from(x)

  implicit def from[T](x: T)(implicit conv: From[T]): PropPath = PropPath(x)

  val empty: PropPath = PropPath(collection.immutable.Queue.empty[PropKey])


  trait From[T] {
    def from(x: T): PropPath
  }


  implicit object FromPropPath extends From[PropPath] {
    def from(x: PropPath) = x
  }

  implicit object FromPropKey extends From[PropKey] {
    def from(x: PropKey) = empty % x
  }

  implicit object FromInt extends From[Int] {
    def from(x: Int) = FromPropKey.from(PropKey(x))
  }

  implicit object FromLong extends From[Long] {
    def from(x: Long) = FromPropKey.from(PropKey(x))
  }

  implicit object FromSymbol extends From[Symbol] {
    def from(x: Symbol) = FromPropKey.from(PropKey(x))
  }

  implicit object FromString extends From[String] {
    def from(x: String) = empty % x
  }
}



sealed abstract class PropVal {
  def value: Any

  def as[T](implicit conv: PropVal.To[T]) = conv.to(this)

  def isNone: Boolean = false
  def asNone: None.type = throw new UnsupportedOperationException(this.getClass.getName + " cannot interpreted as a None")

  def asBoolean: Boolean = throw new UnsupportedOperationException(this.getClass.getName + " cannot interpreted as a Boolean")
  def asLong: Long = throw new UnsupportedOperationException(this.getClass.getName + " cannot interpreted as a Long")
  def asDouble: Double = throw new UnsupportedOperationException(this.getClass.getName + " cannot interpreted as a Double")
  def asString: String = throw new UnsupportedOperationException("PropVals of type " + this.getClass.getName + " cannot be interpreted as a String")
  def asSymbol: Symbol = throw new UnsupportedOperationException("PropVals of type " + this.getClass.getName + " cannot be interpreted as a Symbol")
  def asByteString: ByteString = throw new UnsupportedOperationException("PropVals of type " + this.getClass.getName + " cannot interpreted as a ByteString")
  def asSeq: Seq[PropVal] = throw new UnsupportedOperationException("PropVals of type " + this.getClass.getName + " cannot interpreted as a Seq")
  def asMap: SortedMap[PropKey, PropVal] = throw new UnsupportedOperationException("PropVals of type " + this.getClass.getName + " cannot interpreted as a SortedMap")

  def toJsValue: JsValue

  def asByte: Byte = asLong.toByte
  def asShort: Short = asLong.toShort
  def asInt: Int = asLong.toInt
  def asFloat: Float = asDouble.toFloat

  
  def apply(path: PropPath): PropVal = throw new UnsupportedOperationException("PropVals of type " + this.getClass.getName + " have no properties")
  def apply(path: String): PropVal = apply(PropPath(path))
  def apply(key: Int): PropVal = apply(key.toString)
  def apply(key: Symbol): PropVal = apply(key.name)
  
  def toJSON = toJsValue.toString

  override def toString = toJSON
}


object PropVal {
  def apply[T](x: T)(implicit conv: From[T]) = conv.from(x)

  implicit def from[T](x: T)(implicit conv: From[T]): PropVal = PropVal(x)


  trait From[-T] {
    def from(x: T): PropVal
  }


  trait To[+T] {
    def to(propVal: PropVal): T
  }


  trait Converter[T] extends From[T] with To[T]


  implicit object PropValConverter extends Converter[PropVal] {
    def from(x: PropVal) = x
    def to(propVal: PropVal) = propVal
  }

  implicit object NoneConverter extends Converter[None.type] {
    def from(x: None.type) = NonePropVal
    def to(propVal: PropVal) = propVal.asNone
  }

  implicit object BooleanConverter extends Converter[Boolean] {
    def from(x: Boolean) = BoolPropVal(x)
    def to(propVal: PropVal) = propVal.asBoolean
  }

  implicit object ByteConverter extends Converter[Byte] {
    def from(x: Byte) = IntegerPropVal(x)
    def to(propVal: PropVal) = propVal.asByte
  }

  implicit object ShortConverter extends Converter[Short] {
    def from(x: Short) = IntegerPropVal(x)
    def to(propVal: PropVal) = propVal.asShort
  }

  implicit object IntConverter extends Converter[Int] {
    def from(x: Int) = IntegerPropVal(x)
    def to(propVal: PropVal) = propVal.asInt
  }

  implicit object LongConverter extends Converter[Long] {
    def from(x: Long) = IntegerPropVal(x)
    def to(propVal: PropVal) = propVal.asLong
  }

  implicit object FloatConverter extends Converter[Float] {
    def from(x: Float) = RealPropVal(x)
    def to(propVal: PropVal) = propVal.asFloat
  }

  implicit object DoubleConverter extends Converter[Double] {
    def from(x: Double) = RealPropVal(x)
    def to(propVal: PropVal) = propVal.asDouble
  }

  implicit object SymbolConverter extends Converter[Symbol] {
    def from(x: Symbol) = SymbolPropVal(x)
    def to(propVal: PropVal) = propVal.asSymbol
  }

  implicit object StringConverter extends Converter[String] {
    def from(x: String) = StringPropVal(x)
    def to(propVal: PropVal) = propVal.asString
  }

  implicit object ByteStringConverter extends Converter[ByteString] {
    def from(x: ByteString) = BytesPropVal(x)
    def to(propVal: PropVal) = propVal.asByteString
  }

  implicit object SeqConverter extends Converter[Seq[PropVal]] {
    def from(x: Seq[PropVal]) = PropValSeq(x: _*)
    def to(propVal: PropVal) = propVal.asSeq
  }


  implicit object JSValueConverter extends Converter[JsValue] {
    protected val stringDataTag = "data:,"

    def from(x: JsValue) = x match {
      case JsNull => NonePropVal
      case JsBoolean(x) => BoolPropVal(x)
      case JsNumber(x) => if (x.isWhole) IntegerPropVal(x.longValue) else RealPropVal(x.doubleValue)
      case JsString(x) => {
        if (x startsWith stringDataTag) BytesPropVal(ByteString( Base64.decodeBase64(x substring stringDataTag.length) ))
        else StringPropVal(x)
      }
      case JsArray(x) => {
        import scala.collection.breakOut
        val elems: Vector[PropVal] = x.map{v => PropVal.fromJsValue(v)}(breakOut)
        PropValSeq(elems: _*)
      }
      case x: JsObject => Props.fromJsObject(x)
      case u: JsUndefined => throw new RuntimeException("Encountered JsUndefined, " + u.error)
    }

    def to(propVal: PropVal) = propVal.toJsValue
  }


  implicit object PropValJSValueFormat extends Format[PropVal] {
    def reads(json: JsValue): JsResult[PropVal] = {
      try JsSuccess(JSValueConverter.from(json))
      catch {
        case error: Exception => JsError(error.getMessage)
      }
    }

    def writes(o: PropVal): JsValue = o.toJsValue
  }


  def fromJsValue(value: JsValue): PropVal = JSValueConverter.from(value)

  def fromJSON(json: String): PropVal = fromJsValue(Json.parse(json))
}


case object NonePropVal extends PropVal {
  def value: None.type = None
  override def isNone = true
  override def asNone = value
  def toJsValue = JsNull
}


final case class BoolPropVal(value: Boolean) extends PropVal {
  override def asBoolean = value
  override def asLong = if (value) 1 else 0
  override def asDouble = if (value) 1.0 else 0.0
  def toJsValue: JsBoolean = JsBoolean(value)
}


abstract class NumericPropVal extends PropVal


object NumericPropVal {
  def apply(x: Int): NumericPropVal = IntegerPropVal(x)
  def apply(x: Long): NumericPropVal = IntegerPropVal(x)
  def apply(x: Double): NumericPropVal = {
    val lx = x.toLong
    if (x != lx) RealPropVal(x) else IntegerPropVal(lx)
  }
}


final case class IntegerPropVal(value: Long) extends NumericPropVal {
  override def asBoolean = (value >= 0)
  override def asLong = value
  override def asDouble = value.toDouble
  def toJsValue: JsNumber = JsNumber(value)
  override def toString = value.toString
}


final case class RealPropVal(value: Double) extends NumericPropVal {
  override def asBoolean = (value >= 0)
  override def asLong = value.toLong
  override def asDouble = value
  def toJsValue: JsNumber = JsNumber(value)
}


final case class BytesPropVal(value: ByteString) extends PropVal {
  override def asByteString = value
  def toJsValue: JsString = JsString(toString)
  override def toString = "data:," + new String(Base64.encodeBase64(value.toArray))
}


final case class SymbolPropVal(value: Symbol) extends PropVal {
  override def asSymbol = value
  override def asString = value.name
  def toJsValue: JsString = JsString(value.name)
  override def toString = asString
}


final case class StringPropVal(value: String) extends PropVal {
  override def asString = value
  def toJsValue: JsString = JsString(value)
  override def toString = value
}



abstract class ComplexPropVal extends PropVal {
}



final case class PropValSeq(value: PropVal*) extends ComplexPropVal {
  override def asSeq = value
  def toJsValue = JsArray(value.toList map {_.toJsValue})
}


final case class Props(value: SortedMap[PropKey, PropVal])
  extends ComplexPropVal
  with scala.collection.immutable.SortedMap[PropKey, PropVal]
  with scala.collection.SortedMapLike[PropKey, PropVal, Props]
  with scala.collection.immutable.MapLike[PropKey, PropVal, Props]
  with Serializable
{
  // Declared in scala.collection.MapLike:
  def -(key: PropKey): Props = Props(value - key)
  def get(key: PropKey): Option[PropVal] = value.get(key)
  def iterator: Iterator[(PropKey, PropVal)] = value.iterator

  // Declared in scala.collection.generic.Sorted:
  def keysIteratorFrom(start: PropKey): Iterator[PropKey] = value.keysIteratorFrom(start)

  // Declared in scala.collection.SortedMapLike:
  def iteratorFrom(start: PropKey): Iterator[(PropKey, PropVal)] = value.iteratorFrom(start)
  implicit def ordering: Ordering[PropKey] = value.ordering
  def rangeImpl(from: Option[PropKey], until: Option[PropKey]): Props = Props(value.rangeImpl(from, until))
  def valuesIteratorFrom(start: PropKey): Iterator[PropVal] = value.valuesIteratorFrom(start)


  override def empty = Props.empty
  override protected def newBuilder = Props.newBuilder

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

  override def isEmpty: Boolean = value.isEmpty

  override def stringPrefix = "Props"
  override def toString = toJSON

  def ++(xs: GenTraversableOnce[(PropKey, PropVal)]): Props = Props(value ++ xs)

  def ++! (xs: GenTraversableOnce[(PropKey, PropVal)]): Props = patchMerge(xs, false)
  def ++& (xs: GenTraversableOnce[(PropKey, PropVal)]): Props = patchMerge(xs, true)

  //def +!(kv: (PropKey, PropVal)): Props = Props(value + kv)


  def +[K, V](kv: (K, V))(implicit kFrom: PropKey.From[K], vFrom: PropVal.From[V]): Props = {
    val propKey = kFrom.from(kv._1)
    val propVal = vFrom.from(kv._2)
    Props(value + ((propKey, propVal)))
  }


  def +![K, V](kv: (K, V))(implicit kFrom: PropKey.From[K], vFrom: PropVal.From[V]): Props = {
    val propKey = kFrom.from(kv._1)
    val newValue = vFrom.from(kv._2)

    newValue match {
      case newProps: Props => this.value.get(propKey) match {
        case Some(oldProps: Props) => this + ((propKey, oldProps ++! newProps))
        case _ => this + ((propKey, newValue))
      }
      case _ => this + ((propKey, newValue))
    }
  }


  override def asMap = value
  def toJsValue = JsObject(value.toList map {case (k,v) => (k.toString, v.toJsValue)})

  protected def patchMerge(xs: GenTraversableOnce[(PropKey, PropVal)], merge: Boolean): Props = {
    if (xs.isEmpty) this
    else Props(
      xs.foldLeft(this.value) { case (result, (k, vNew)) =>
        result.get(k) match {
          case Some(vOld) => (vOld, vNew) match {
            case (vOld: Props, vNew: Props) =>
              result + (( k, vOld.patchMerge(vNew.value, merge) ))
            case (vOld, vNew) =>
              if (merge) {
                if (vOld == vNew) result
                else throw new IllegalArgumentException("Can't merge Props with conflicting contents")
              } else {
                result + { val e = (k, vNew); e }
              }
          }
          case None => result + { val e = (k, vNew); e }
        }
      }
    )
  }
}


object Props {
  val empty = Props(SortedMap.empty[PropKey, PropVal])

  def newBuilder = SortedMap.newBuilder[PropKey, PropVal] mapResult {Props(_)}

  def apply(values: (PropPath, PropVal)*): Props = {
    values.foldLeft(Props.empty) { case (props, (path, value)) =>
      props ++! (
        path.parts.reverse.foldLeft(value) {
          case (v, k) => Props(SortedMap(k -> v))
        }.asInstanceOf[Props]
      )
    }
  }

  def fromJsObject(obj: JsObject) =
    new Props( obj.fields.map{ case (k,v) => (PropKey(k), PropVal.fromJsValue(v)) }(breakOut) )
  
  def fromJSON(json: String) = PropVal.fromJSON(json).asInstanceOf[Props]
}


object PropsTest {
  Props('foo -> 42) +! ('bar, 33)
  Props('foo -> 42) +! ('bar -> 33)
  Props('foo -> 42) ++! Props('bar -> 33)
}
