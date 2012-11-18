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

import scala.reflect.{ClassTag, classTag}


package object util {

type ByteString = akka.util.ByteString
val  ByteString = akka.util.ByteString
type ByteIterator = akka.util.ByteIterator
val  ByteIterator = akka.util.ByteIterator
type ByteStringBuilder = akka.util.ByteStringBuilder
object ByteStringBuilder { def apply() = ByteString.newBuilder }

type Timeout = akka.util.Timeout
val  Timeout = akka.util.Timeout

implicit def iteratorOps[A](it: Iterator[A])  = new IteratorOps(it)
implicit def iterableOps[A](coll: Iterable[A]) = new IterableOps(coll)
implicit def traversableOnceOps[A](coll: TraversableOnce[A]) = new TraversableOnceOps(coll)
implicit def nestedSeqOps[A: ClassTag](seq: Seq[Seq[A]]) = new NestedSeqOps(seq)
implicit def seqOps[A: ClassTag](seq: Seq[A]) = new SeqOps(seq)
implicit def arrayOps[A: ClassTag](array: Array[A]) = new ArrayOps(array)

implicit def urlOps(url: java.net.URL) = new URLOps(url)

implicit def randomOps(rnd: scala.util.Random) = new RandomOps(rnd)
implicit val defaultRandom = scala.util.Random


implicit def intArrayIteratorOpt(it: ArrayIterator[Int]) = new IntArrayIteratorOpt(it)
implicit def longArrayIteratorOpt(it: ArrayIterator[Long]) = new LongArrayIteratorOpt(it)
implicit def floatArrayIteratorOpt(it: ArrayIterator[Float]) = new FloatArrayIteratorOpt(it)
implicit def doubleArrayIteratorOpt(it: ArrayIterator[Double]) = new DoubleArrayIteratorOpt(it)

implicit def intArrayVecOpt(seq: ArrayVec[Int]) = new IntArrayVecOpt(seq)
implicit def longArrayVecOpt(seq: ArrayVec[Long]) = new LongArrayVecOpt(seq)
implicit def floatArrayVecOpt(seq: ArrayVec[Float]) = new FloatArrayVecOpt(seq)
implicit def doubleArrayVecOpt(seq: ArrayVec[Double]) = new DoubleArrayVecOpt(seq)

implicit def arrayVecToArrayVecBoolean(v: ArrayVec[Boolean]) = new ArrayVecBoolean(v)
implicit def arrayVecToArrayVecByte(v: ArrayVec[Byte]) = new ArrayVecByte(v)
implicit def arrayVecToArrayVecChar(v: ArrayVec[Char]) = new ArrayVecChar(v)
implicit def arrayVecToArrayVecShort(v: ArrayVec[Short]) = new ArrayVecShort(v)
implicit def arrayVecToArrayVecInt(v: ArrayVec[Int]) = new ArrayVecInt(v)
implicit def arrayVecToArrayVecLong(v: ArrayVec[Long]) = new ArrayVecLong(v)
implicit def arrayVecToArrayVecFloat(v: ArrayVec[Float]) = new ArrayVecFloat(v)
implicit def arrayVecToArrayVecDouble(v: ArrayVec[Double]) = new ArrayVecDouble(v)


def fast[A: ClassTag](seq: Seq[A]) = FastSeqOps[A](seq)


implicit def string2PropPath(s: String) = PropPath(s)


def fctResponder[A](x: () => A) = new Responder[A] { def respond(k: A => Unit) = k(x()) }


def classTagFrom(a: Any): ClassTag[_] = a match {
  case a:Boolean => classTag[Boolean]
  case a:Byte => classTag[Byte]
  case a:Char => classTag[Char]
  case a:Short => classTag[Short]
  case a:Int => classTag[Int]
  case a:Long => classTag[Long]
  case a:Float => classTag[Float]
  case a:Double => classTag[Double]
  case a:Unit => classTag[Unit]
  case a:AnyRef => scala.reflect.ClassTag(a.getClass)
}


def genClassTag[A: ClassTag](a: A) = classTag[A]


def as[A](x:Any) = x.asInstanceOf[A]


def sizeOf[A <: AnyVal : ClassTag]: Int = {
  val ct = classTag[A]

  if (ct == classTag[Byte])         1
  else if (ct == classTag[Char])    2
  else if (ct == classTag[Short])   2
  else if (ct == classTag[Int])     4
  else if (ct == classTag[Long])    8
  else if (ct == classTag[Float])   4
  else if (ct == classTag[Double])  8
  else throw new IllegalArgumentException("sizeOf() does not support %s".format(ct))
}


def hex(v: AnyVal) : String = {
  def byteMF = classTag[Byte]

  v match {
    case x: Byte     =>  "%02x".format(x)
    case x: Short    =>  "%04x".format(x)
    case x: Int      =>  "%08x".format(x)
    case x: Long     =>  "%016x".format(x)
    case x: Boolean  =>  if (x) "1" else "0"
    case _ => throw new IllegalArgumentException("hex() does not support %s".format(classTagFrom(v)))
  }
}


def floorLog2(x: Int) = 8 * sizeOf[Int] - 1 - java.lang.Integer.numberOfLeadingZeros(x)
def ceilLog2(x: Int) = 8 * sizeOf[Int] - java.lang.Integer.numberOfLeadingZeros(x-1)


def currentTime: Double = java.lang.System.currentTimeMillis * 1e-3


def timedExec[T](body: => T): (T, (Double, Double)) = {
  val tmxb = java.lang.management.ManagementFactory.getThreadMXBean()
  val threadId = Thread.currentThread().getId()
  val t1 = java.lang.System.nanoTime
  val u1 = tmxb.getThreadUserTime(threadId)
  val res = body
  val t2 = java.lang.System.nanoTime
  val u2 = tmxb.getThreadUserTime(threadId)
  val totalTime = (t2 - t1) * 1E-9d
  val totalUserTime = (u2 - u1) * 1E-9d
  (res, (totalTime, totalUserTime))
}


def timedExecN[T](n: Int)(body: => T): (Double, Double) =
  timedExec{ for (i <- 1 to n) (body) }._2


def timedExecMean[T](n: Int)(body: => T): (Double, Double) = {
  val (total, user) = timedExecN(n)(body)
  (total / n * 1E6, user / n * 1E6)
}


def loggable(a: Any) = {
  val s = a.toString
  val shortened = (if (s.size > 40) s.take(40) + " ..." else s)
  shortened collect { case '\n' | '\r' => ' '; case c => c }
}


def localHostName: String = java.net.InetAddress.getLocalHost.getHostName

}
