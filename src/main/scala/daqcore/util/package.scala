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


package daqcore


package object util {


implicit def iteratorOps[A](it: Iterator[A])  = new IteratorOps(it)
implicit def iterableOps[A](coll: Iterable[A]) = new IterableOps(coll)
implicit def traversableOnceOps[A](coll: TraversableOnce[A]) = new TraversableOnceOps(coll)


implicit def idxSeq2subSeq[T](seq: IndexedSeq[T]) = new SubIdxSeq(seq, 0, seq.length)


def fctResponder[A](x: () => A) = new Responder[A] { def respond(k: A => Unit) = k(x()) }


def classMF(a: Any): ClassManifest[_] = a match {
  case a:Boolean => classManifest[Boolean]
  case a:Byte => classManifest[Byte]
  case a:Char => classManifest[Char]
  case a:Short => classManifest[Short]
  case a:Int => classManifest[Int]
  case a:Long => classManifest[Long]
  case a:Float => classManifest[Float]
  case a:Double => classManifest[Double]
  case a:Unit => classManifest[Unit]
  case a:AnyRef => scala.reflect.ClassManifest.fromClass(a.getClass)
}


def as[A](x:Any) = x.asInstanceOf[A]


def sizeOf[A <: AnyVal : ClassManifest]: Int = {
  val mf = classManifest[A]

  if (mf == classManifest[Byte])         1
  else if (mf == classManifest[Char])    2
  else if (mf == classManifest[Short])   2
  else if (mf == classManifest[Int])     4
  else if (mf == classManifest[Long])    8
  else if (mf == classManifest[Float])   4
  else if (mf == classManifest[Double])  8
  else throw new IllegalArgumentException("sizeOf() does not support %s".format(mf))
}


def hex(v: AnyVal) : String = {
  def byteMF = classManifest[Byte]

  v match {
    case x: Byte     =>  "%02X".format(x)
    case x: Short    =>  "%04X".format(x)
    case x: Int      =>  "%08X".format(x)
    case x: Long     =>  "%016X".format(x)
    case x: Boolean  =>  if (x) "1" else "0"
    case _ => throw new IllegalArgumentException("hex() does not support %s".format(classMF(v)))
  }
}


def floorLog2(x: Int) = 8 * sizeOf[Int] - 1 - java.lang.Integer.numberOfLeadingZeros(x)
def ceilLog2(x: Int) = 8 * sizeOf[Int] - java.lang.Integer.numberOfLeadingZeros(x-1)


def currentTime: Double = java.lang.System.currentTimeMillis * 1e-3

}
