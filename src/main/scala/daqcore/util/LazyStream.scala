// Based on Scala's original Stream class, SVN r22108
// LazyStream is thread-safe.

package daqcore.util

import scala.collection.{LinearSeq, LinearSeqOptimized}
import scala.collection.generic._
import scala.collection.mutable.{Builder, StringBuilder, LazyBuilder, ListBuffer}
import scala.annotation.tailrec

/** The class `LazyStream` implements lazy lists where elements
 *  are only evaluated when they are needed. Here is an example:
 * 
 *  {{{
 *  object Main extends Application {
 *    
 *    def from(n: Int): LazyStream[Int] =
 *      LazyStream.cons(n, from(n + 1))
 *    
 *    def sieve(s: LazyStream[Int]): LazyStream[Int] =
 *      LazyStream.cons(s.head, sieve(s.tail filter { _ % s.head != 0 }))
 *    
 *    def primes = sieve(from(2))
 *    
 *    primes take 10 print
 *  }
 *  }}}
 *  
 *  @tparam A    the type of the elements contained in this LazyStream.
 *  
 *  @author Martin Odersky, Matthias Zenger, modified by Oliver Schulz
 *  @version 1.1 08/08/03
 *  @since   2.8
 *  @define Coll LazyStream
 *  @define coll LazyStream
 *  @define orderDependent
 *  @define orderDependentFold
 */
abstract class LazyStream[+A] extends LinearSeq[A] 
                             with GenericTraversableTemplate[A, LazyStream]
                             with LinearSeqOptimized[A, LazyStream[A]] {
self =>
  override def companion: GenericCompanion[LazyStream] = LazyStream

  import scala.collection.{Traversable, Iterable, Seq, IndexedSeq}

  /** is this LazyStream empty? */
  def isEmpty: Boolean

  /** The first element of this LazyStream 
   *  @throws Predef.NoSuchElementException if the LazyStream is empty.
   */
  def head: A

  /** A LazyStream consisting of the remaining elements of this LazyStream after the first one.
   *  @throws Predef.UnsupportedOperationException if the LazyStream is empty.
   */
  def tail: LazyStream[A]

  /** Is the head of this LazyStream defined? */
  protected def headDefined: Boolean

  // Implementation of abstract method in Traversable

  // New methods in LazyStream
  
  /** The LazyStream resulting from the concatenation of this LazyStream with the argument LazyStream.
   *  @param rest   The LazyStream that gets appended to this LazyStream
   *  @return       The LazyStream containing elements of this LazyStream and the traversable object.
   */
  def append[B >: A](rest: => Traversable[B]): LazyStream[B] =
    if (isEmpty) rest.toLazyStream else new LazyStream.Cons(head, tail append rest)

  /** Forces evaluation of the whole LazyStream and returns it. */
  def force: LazyStream[A] = {
    var these = this
    while (!these.isEmpty) these = these.tail
    this
  }

  /** Prints elements of this LazyStream one by one, separated by commas. */
  def print() { print(", ") }

  /** Prints elements of this LazyStream one by one, separated by `sep`.
   *  @param sep   The separator string printed between consecutive elements. 
   */
  def print(sep: String) {
    def loop(these: LazyStream[A], start: String) {
      Console.print(start)
      if (these.isEmpty) Console.print("empty")
      else { 
        Console.print(these.head)
        loop(these.tail, sep) 
      }
    }
    loop(this, "")
  }
  
  override def length: Int = {
    var len = 0
    var left = this
    while (!left.isEmpty) {
      len += 1
      left = left.tail
    }
    len
  }
  
  /** It's an imperfect world, but at least we can bottle up the
   *  imperfection in a capsule.
   */
  @inline private def asThat[That](x: AnyRef): That     = x.asInstanceOf[That]
  @inline private def asLazyStream[B](x: AnyRef): LazyStream[B] = x.asInstanceOf[LazyStream[B]]

  // Overridden methods from Traversable
  
  def toLazyStream: LazyStream[A] = this

  override def hasDefiniteSize = {
    def loop(s: LazyStream[A]): Boolean = s.isEmpty || s.headDefined && loop(s.tail)
    loop(this)
  }

  /** Create a new LazyStream which contains all elements of this LazyStream
   *  followed by all elements of Traversable `that`.
   *  @note It's subtle why this works. We know that if the target type
   *  of the Builder That is either a LazyStream, or one of its supertypes, or undefined,
   *  then LazyStreamBuilder will be chosen for the implicit.
   *  we recognize that fact and optimize to get more laziness. 
   */
  override def ++[B >: A, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[LazyStream[A], B, That]): That =
    // we assume there is no other builder factory on LazyStreams and therefore know that That = LazyStream[A]
    asThat[That](
      if (isEmpty) that.toLazyStream
      else new LazyStream.Cons(head, asLazyStream[A](tail ++ that))
    )
  
  /**
   * Create a new LazyStream which contains all intermediate results of applying the operator
   * to subsequent elements left to right.
   * @note This works because the target type of the Builder That is a LazyStream.
   */
  override final def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[LazyStream[A], B, That]): That =
    asThat[That](
      if (isEmpty) LazyStream(z)
      else new LazyStream.Cons(z, asLazyStream[B](tail.scanLeft(op(z, head))(op)))
    )

  /** Returns the LazyStream resulting from applying the given function
   *  `f` to each element of this LazyStream.
   *
   *  @param f function to apply to each element.
   *  @return  <code>f(a<sub>0</sub>), ..., f(a<sub>n</sub>)</code> if this
   *           sequence is <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  override final def map[B, That](f: A => B)(implicit bf: CanBuildFrom[LazyStream[A], B, That]): That =
    asThat[That](
      if (isEmpty) LazyStream.Empty
      else new LazyStream.Cons(f(head), asLazyStream[B](tail map f))
    )
    
  /** Applies the given function `f` to each element of
   *  this LazyStream, then concatenates the results.
   *
   *  @param f  the function to apply on each element.
   *  @param bf $bfinfo
   *  @return  <code>f(a<sub>0</sub>) ::: ... ::: f(a<sub>n</sub>)</code> if
   *           this LazyStream is <code>[a<sub>0</sub>, ..., a<sub>n</sub>]</code>.
   */
  override final def flatMap[B, That](f: A => Traversable[B])(implicit bf: CanBuildFrom[LazyStream[A], B, That]): That =
    // we assume there is no other builder factory on LazyStreams and therefore know that That = LazyStream[B]
    // optimisations are not for speed, but for functionality
    // see tickets #153, #498, #2147, and corresponding tests in run/ (as well as run/LazyStream_flatmap_odds.scala)
    asThat[That](
      if (isEmpty) LazyStream.Empty
      else {
        // establish !prefix.isEmpty || nonEmptyPrefix.isEmpty
        var nonEmptyPrefix = this
        var prefix = f(nonEmptyPrefix.head).toLazyStream
        while (!nonEmptyPrefix.isEmpty && prefix.isEmpty) {
          nonEmptyPrefix = nonEmptyPrefix.tail
          if(!nonEmptyPrefix.isEmpty)
            prefix = f(nonEmptyPrefix.head).toLazyStream
        }

        if (nonEmptyPrefix.isEmpty) LazyStream.empty
        else prefix append asLazyStream[B](nonEmptyPrefix.tail flatMap f)
      }
    )

  /** Returns all the elements of this LazyStream that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *
   *  @param p the predicate used to filter the LazyStream.
   *  @return the elements of this LazyStream satisfying <code>p</code>.
   */
  override final def filter(p: A => Boolean): LazyStream[A] = {
    // optimization: drop leading prefix of elems for which f returns false
    var rest = this dropWhile (!p(_))
    if (rest.isEmpty) LazyStream.Empty
    else new LazyStream.Cons(rest.head, rest.tail filter p)
  }
  
  override final def withFilter(p: A => Boolean): LazyStreamWithFilter = new LazyStreamWithFilter(p)

  /** A lazier implementation of WithFilter than TraversableLike's.
   */
  final class LazyStreamWithFilter(p: A => Boolean) extends WithFilter(p) {    
    override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[LazyStream[A], B, That]): That = {
      def tailMap = asLazyStream[B](tail withFilter p map f)
      asThat[That](
        if (isEmpty) LazyStream.Empty
        else if (p(head)) new LazyStream.Cons(f(head), tailMap)
        else tailMap
      )
    }
    
    override def flatMap[B, That](f: A => Traversable[B])(implicit bf: CanBuildFrom[LazyStream[A], B, That]): That = {
      def tailFlatMap = asLazyStream[B](tail withFilter p flatMap f) 
      asThat[That](
        if (isEmpty) LazyStream.Empty
        else if (p(head)) f(head).toLazyStream append tailFlatMap
        else tailFlatMap
      )
    }

    override def foreach[B](f: A => B) =
      for (x <- self) 
        if (p(x)) f(x)
    
    override def withFilter(q: A => Boolean): LazyStreamWithFilter = 
      new LazyStreamWithFilter(x => p(x) && q(x))
  }

  /** Apply the given function <code>f</code> to each element of this linear sequence
   *  (while respecting the order of the elements).
   *
   *  @param f the treatment to apply to each element.
   *  @note  Overridden here as final to trigger tail-call optimization, which replaces
   *         'this' with 'tail' at each iteration. This is absolutely necessary
   *         for allowing the GC to collect the underlying LazyStream as elements are
   *         consumed.
   */
  @tailrec
  override final def foreach[B](f: A => B) {
    if (!this.isEmpty) {
      f(head)
      tail.foreach(f)
    }
  }
  
  /** LazyStream specialization of foldLeft which allows GC to collect
   *  along the way.
   */
  @tailrec
  override final def foldLeft[B](z: B)(op: (B, A) => B): B = {
    if (this.isEmpty) z
    else tail.foldLeft(op(z, head))(op)
  }

  /** Returns all the elements of this LazyStream that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *
   *  @param p the predicate used to filter the LazyStream.
   *  @return the elements of this LazyStream satisfying <code>p</code>.
   */
  override def partition(p: A => Boolean): (LazyStream[A], LazyStream[A]) = (filter(p(_)), filterNot(p(_)))
    
  /** Returns a LazyStream formed from this LazyStream and the specified LazyStream
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *  If one of the two LazyStreams is longer than the other, its remaining elements are ignored.
   *
   *  @return     <code>LazyStream({a<sub>0</sub>,b<sub>0</sub>}, ...,
   *              {a<sub>min(m,n)</sub>,b<sub>min(m,n)</sub>)}</code> when
   *              <code>LazyStream(a<sub>0</sub>, ..., a<sub>m</sub>)
   *              zip LazyStream(b<sub>0</sub>, ..., b<sub>n</sub>)</code> is invoked.
   */
  override final def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: CanBuildFrom[LazyStream[A], (A1, B), That]): That =
    // we assume there is no other builder factory on LazyStreams and therefore know that That = LazyStream[(A1, B)]
    asThat[That](
      if (this.isEmpty || that.isEmpty) LazyStream.Empty
      else new LazyStream.Cons((this.head, that.head), asLazyStream[(A1, B)](this.tail zip that.tail))
    )

  /** Zips this iterable with its indices. `s.zipWithIndex` is equivalent to 
   *  `s zip s.indices`
   */
  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[LazyStream[A], (A1, Int), That]): That =
    this.zip[A1, Int, That](LazyStream.from(0))
  
  /** Write all defined elements of this iterable into given string builder.
   *  The written text begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of defined elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>. The method will not force evaluation of undefined elements. A
   *  tail of such elements will be represented by a "?" instead.
   */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    def loop(pre: String, these: LazyStream[A]) {
      if (these.isEmpty) b append end
      else {
        if (these.headDefined) {
          b append pre append these.head
          loop(sep, these.tail)
        }
        else b append pre append "?"  append end
      }
    }
    b append start
    loop("", this)
    b
  }

  override def mkString(start: String, sep: String, end: String): String = {
    this.force
    super.mkString(start, sep, end)
  }

  override def mkString(sep: String): String = {
    this.force
    super.mkString(sep)
  }

  override def mkString: String = {
    this.force
    super.mkString
  }

  override def toString = super.mkString(stringPrefix + "(", ", ", ")")

  /** Returns the <code>n</code> first elements of this LazyStream, or else the whole 
   *  LazyStream, if it has less than <code>n</code> elements.
   *
   *  @param n the number of elements to take.
   *  @return the <code>n</code> first elements of this LazyStream.
   */
  override def take(n: Int): LazyStream[A] =
    if (n <= 0 || isEmpty) LazyStream.Empty
    else new LazyStream.Cons(head, if (n == 1) LazyStream.empty else tail take (n-1))

  /** A subLazyStream starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @note This is equivalent to (but possibly more efficient than)
   *  c.drop(from).take(to - from)
   *
   *  @param start   The index of the first element of the returned subsequence
   *  @param end     The index of the element following the returned subsequence
   *  @throws IndexOutOfBoundsException if <code>from &lt; 0</code>
   *          or <code>length &lt; from + len<code>
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  override def slice(start: Int, end: Int): LazyStream[A] = {
    var len = end
    if (start > 0) len -= start
    drop(start) take len
  }

  /** The LazyStream without its last element.
   *  @throws Predef.UnsupportedOperationException if the LazyStream is empty.
   */
  override def init: LazyStream[A] =
    if (isEmpty) super.init
    else if (tail.isEmpty) LazyStream.Empty
    else new LazyStream.Cons(head, tail.init)

  /** Returns the rightmost <code>n</code> elements from this iterable.
   *  @param n the number of elements to take
   */
  override def takeRight(n: Int): LazyStream[A] = {
    var these: LazyStream[A] = this
    var lead = this drop n
    while (!lead.isEmpty) {
      these = these.tail
      lead = lead.tail
    }
    these
  }

  // there's nothing we can do about dropRight, so we just keep the definition in LinearSeq
  
  /** Returns the longest prefix of this LazyStream whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   */
  override def takeWhile(p: A => Boolean): LazyStream[A] =
    if (!isEmpty && p(head)) new LazyStream.Cons(head, tail takeWhile p) 
    else LazyStream.Empty

  /** Returns the longest suffix of this iterable whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   */
  override def dropWhile(p: A => Boolean): LazyStream[A] = {
    var these: LazyStream[A] = this
    while (!these.isEmpty && p(these.head)) these = these.tail
    these
  }

  /** Builds a new LazyStream from this LazyStream in which any duplicates (wrt to ==) removed.
   *  Among duplicate elements, only the first one is retained in the result LazyStream
   */
  override def distinct: LazyStream[A] =
    if (isEmpty) this
    else new LazyStream.Cons(head, tail.filter(head !=).distinct)

  /** Returns a new sequence of given length containing the elements of this sequence followed by zero
   *  or more occurrences of given elements. 
   */
  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[LazyStream[A], B, That]): That = {
    def loop(len: Int, these: LazyStream[A]): LazyStream[B] = 
      if (these.isEmpty) LazyStream.fill(len)(elem)
      else new LazyStream.Cons(these.head, loop(len - 1, these.tail))
    
    asThat[That](loop(len, this))
// was:    if (bf.isInstanceOf[LazyStream.LazyStreamCanBuildFrom[_]]) loop(len, this).asInstanceOf[That] 
//    else super.padTo(len, elem)
  }

  /** A list consisting of all elements of this list in reverse order.
   */
  override def reverse: LazyStream[A] = {
    var result: LazyStream[A] = LazyStream.Empty
    var these = this
    while (!these.isEmpty) {
      val r = LazyStream.consWrapper(result).#::(these.head)
      r.tail // force it!
      result = r
      these = these.tail
    }
    result
  }

  override def flatten[B](implicit asTraversable: A => /*<:<!!!*/ Traversable[B]): LazyStream[B] = {
    def flatten1(t: Traversable[B]): LazyStream[B] =
      if (!t.isEmpty)
        new LazyStream.Cons(t.head, flatten1(t.tail))
      else
        tail.flatten

    if (isEmpty)
      LazyStream.empty
    else
      flatten1(asTraversable(head))
  }

  /** Defines the prefix of this object's <code>toString</code> representation as ``LazyStream''.
   */
  override def stringPrefix = "LazyStream"
}

/**
 * The object <code>LazyStream</code> provides helper functions
 * to manipulate LazyStreams.
 *
 * @author Martin Odersky, Matthias Zenger
 * @version 1.1 08/08/03
 * @since   2.8
 */
object LazyStream extends SeqFactory[LazyStream] {
  
  /** The factory for LazyStreams.
   *  @note Methods such as map/flatMap will not invoke the Builder factory,
   *        but will return a new LazyStream directly, to preserve laziness.
   *        The new LazyStream is then cast to the factory's result type.
   *        This means that every CanBuildFrom that takes a
   *        LazyStream as its From type parameter must yield a LazyStream as its result parameter.
   *        If that assumption is broken, cast errors might result.
   */
  class LazyStreamCanBuildFrom[A] extends GenericCanBuildFrom[A]

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LazyStream[A]] = new LazyStreamCanBuildFrom[A]

  /** Creates a new builder for a LazyStream */
  def newBuilder[A]: Builder[A, LazyStream[A]] = new LazyStreamBuilder[A]

  import scala.collection.{Iterable, Seq, IndexedSeq}

  /** A builder for LazyStreams
   *  @note This builder is lazy only in the sense that it does not go downs the spine
   *        of traversables that are added as a whole. If more laziness can be achieved,
   *        this builder should be bypassed.
   */
  class LazyStreamBuilder[A] extends scala.collection.mutable.LazyBuilder[A, LazyStream[A]] {
    def result: LazyStream[A] = parts.toLazyStream flatMap (_.toLazyStream)
  }

  object Empty extends LazyStream[Nothing] { 
    override def isEmpty = true
    override def head = throw new NoSuchElementException("head of empty LazyStream")
    override def tail = throw new UnsupportedOperationException("tail of empty LazyStream")
    def headDefined = false
    def tailDefined = false
  }

  /** The empty LazyStream */
  override def empty[A]: LazyStream[A] = Empty

  /** A LazyStream consisting of given elements */
  override def apply[A](xs: A*): LazyStream[A] = xs.toLazyStream

  /** A wrapper class that adds `#::` for cons and `#:::` for concat as operations
   *  to LazyStreams.
   */
  class ConsWrapper[A](tl: => LazyStream[A]) {
    def #::(hd: A): LazyStream[A] = new LazyStream.Cons(hd, tl)
    def #:::(prefix: LazyStream[A]): LazyStream[A] = prefix append tl
  }

  /** A wrapper method that adds `#::` for cons and `#::: for concat as operations
   *  to LazyStreams.
   */
  implicit def consWrapper[A](LazyStream: => LazyStream[A]): ConsWrapper[A] = 
    new ConsWrapper[A](LazyStream)

  /** An extractor that allows to pattern match LazyStreams with `#::`.
   */
  object #:: {
    def unapply[A](xs: LazyStream[A]): Option[(A, LazyStream[A])] = 
      if (xs.isEmpty) None
      else Some((xs.head, xs.tail))
  }

  @deprecated("use #:: instead") lazy val lazy_:: = #::

  /** An alternative way of building and matching LazyStreams using LazyStream.cons(hd, tl).
   */
  object cons {

    /** A LazyStream consisting of a given first element and remaining elements 
     *  @param hd   The first element of the result LazyStream
     *  @param tl   The remaining elements of the result LazyStream
     */
    def apply[A](hd: => A, tl: => LazyStream[A]) = new Cons(hd, tl)

    /** Maps a LazyStream to its head and tail */
    def unapply[A](xs: LazyStream[A]): Option[(A, LazyStream[A])] = #::.unapply(xs)
  }

  /** A lazy cons cell, from which LazyStreams are built. */
  @serializable @SerialVersionUID(-7181024819081987783L)
  final class Cons[+A](hd: => A, tl: => LazyStream[A]) extends LazyStream[A] {
    override def isEmpty = false
    protected def headDefined = headDefinedVar
    @volatile private var headDefinedVar = false
    override lazy val head = {
      headDefinedVar = true
      hd
    }
    override lazy val tail: LazyStream[A] = {
      head
      tl
    }
  }

  /** An infinite LazyStream that repeatedly applies a given function to a start value.
   *
   *  @param start the start value of the LazyStream
   *  @param f     the function that's repeatedly applied
   *  @return      the LazyStream returning the infinite sequence of values `start, f(start), f(f(start)), ...`
   */
  def iterate[A](start: A)(f: A => A): LazyStream[A] = new Cons(start, iterate(f(start))(f))

  override def iterate[A](start: A, len: Int)(f: A => A): LazyStream[A] =
    iterate(start)(f) take len

  /**
   * Create an infinite LazyStream starting at <code>start</code>
   * and incrementing by step <code>step</code>
   *
   * @param start the start value of the LazyStream
   * @param step the increment value of the LazyStream
   * @return the LazyStream starting at value <code>start</code>.
   */
  def from(start: Int, step: Int): LazyStream[Int] =
    new Cons(start, from(start+step, step))

  /**
   * Create an infinite LazyStream starting at <code>start</code>
   * and incrementing by 1.
   *
   * @param start the start value of the LazyStream
   * @return the LazyStream starting at value <code>start</code>.
   */
  def from(start: Int): LazyStream[Int] = from(start, 1)

  /**
   * Create an infinite LazyStream containing the given element expression (which is computed for each
   * occurrence)
   *
   * @param elem the element composing the resulting LazyStream
   * @return the LazyStream containing an infinite number of elem
   */
  def continually[A](elem: => A): LazyStream[A] = new Cons(elem, continually(elem))

  override def fill[A](n: Int)(elem: => A): LazyStream[A] = 
    if (n <= 0) Empty else new Cons(elem, fill(n-1)(elem))

  override def tabulate[A](n: Int)(f: Int => A): LazyStream[A] = {
    def loop(i: Int) =
      if (i >= n) Empty else new Cons(f(i), tabulate(i+1)(f))
    loop(0)
  }

  override def range(start: Int, end: Int, step: Int): LazyStream[Int] =
    if (if (step < 0) start <= end else end <= start) Empty
    else new Cons(start, range(start + step, end, step))
  
  /** A LazyStream containing all elements of a given iterator, in the order they are produced.
   *  @param it   The iterator producing the LazyStream's elements
   */
  @deprecated("use it.toLazyStream instead")
  def fromIterator[A](it: Iterator[A]): LazyStream[A] = it.toLazyStream

  /** The concatenation of a sequence of LazyStreams
   */
  @deprecated("use xs.flatten instead")
  def concat[A](xs: Iterable[LazyStream[A]]): LazyStream[A] = concat(xs.iterator)
  
  /** The concatenation of all LazyStreams returned by an iterator
   */
  @deprecated("use xs.toLazyStream.flatten instead")
  def concat[A](xs: Iterator[LazyStream[A]]): LazyStream[A] = xs.toLazyStream.flatten //(conforms[LazyStream[A], scala.collection.Traversable[A]])

  /**
   * Create a LazyStream with element values
   * <code>v<sub>n+1</sub> = step(v<sub>n</sub>)</code>
   * where <code>v<sub>0</sub> = start</code>
   * and elements are in the range between <code>start</code> (inclusive)
   * and <code>end</code> (exclusive)
   * @param start the start value of the LazyStream
   * @param end the end value of the LazyStream
   * @param step the increment function of the LazyStream, must be monotonically increasing or decreasing
   * @return the LazyStream starting at value <code>start</code>.
   */
  @deprecated("use `iterate' instead.")
  def range(start: Int, end: Int, step: Int => Int): LazyStream[Int] =
    iterate(start, end - start)(step)

  /**
   * Create an infinite LazyStream containing the given element.
   *
   * @param elem the element composing the resulting LazyStream
   * @return the LazyStream containing an infinite number of elem
   */
  @deprecated("use `continually' instead")
  def const[A](elem: A): LazyStream[A] = cons(elem, const(elem))

  /** Create a LazyStream containing several copies of an element.
   *
   *  @param n    the length of the resulting LazyStream
   *  @param elem the element composing the resulting LazyStream
   *  @return     the LazyStream composed of n elements all equal to elem
   */
  @deprecated("use fill(n, elem) instead")
  def make[A](n: Int, elem: A): LazyStream[A] = fill(n)(elem)
}


