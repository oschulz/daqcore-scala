// Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
// Copyright (C) 2013 Oliver Schulz <oliver.schulz@tu-dortmund.de>

// Based on code from akka.actio.IO, imported from akka-2.2.0

// This software is licensed under the Apache 2 license, quoted below.
//  
// Copyright 2009-2013 Typesafe Inc. <http://www.typesafe.com>
//  
// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//  
// http://www.apache.org/licenses/LICENSE-2.0
//  
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.


package daqcore.io

import language.higherKinds
import language.postfixOps

import scala.collection.immutable
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.control.NonFatal
import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom

import akka.util.ByteString

import java.io.EOFException


/**
 * IO messages and iteratees.
 *
 * This is still in an experimental state and is subject to change until it
 * has received more real world testing.
 */
object IO {

  final class DivergentIterateeException extends IllegalStateException("Iteratees should not return a continuation when receiving EOF")

  /**
   * Represents part of a stream of bytes that can be processed by an
   * [[Iteratee]].
   */
  sealed trait Input {
    /**
     * Append another Input to this one.
     *
     * If 'that' is an [[EOF]] then it will replace any
     * remaining bytes in this Input. If 'this' is an [[EOF]]
     * then it will be replaced by 'that'.
     */
    def ++(that: Input): Input
  }

  object Chunk {
    /**
     * Represents the empty Chunk
     */
    val empty: Chunk = new Chunk(ByteString.empty)
  }

  /**
   * Part of an [[Input]] stream that contains a chunk of bytes.
   */
  case class Chunk(bytes: ByteString) extends Input {
    final override def ++(that: Input): Input = that match {
      case c @ Chunk(more) ⇒
        if (more.isEmpty) this
        else if (bytes.isEmpty) c
        else Chunk(bytes ++ more)
      case other ⇒ other
    }
  }

  /**
   * Part of an [[Input]] stream that represents the end of the
   * stream.
   *
   * This will cause the [[Iteratee]] that processes it
   * to terminate early.
   */
  case object EOF extends Input { final override def ++(that: Input): Input = that }

  /**
   * Part of an [[Input]] stream that represents an error in the stream.
   *
   * This will cause the [[Iteratee]] that processes it
   * to terminate early.
   */
  case class Error(cause: Throwable) extends Input { final override def ++(that: Input): Input = that }

  object Iteratee {
    /**
     * Wrap the provided value within a [[Done]]
     * [[Iteratee]]. This is a helper for cases where the type should be
     * inferred as an Iteratee and not as a Done.
     */
    def apply[A](value: A): Iteratee[A] = Done(value)

    /**
     * Returns Iteratee.unit
     */
    def apply(): Iteratee[Unit] = unit

    /**
     * The single value representing Done(())
     */
    val unit: Iteratee[Unit] = Done(())
  }

  /**
   * A basic Iteratee implementation of Oleg's Iteratee (http://okmij.org/ftp/Streams.html).
   * To keep this implementation simple it has no support for Enumerator or Input types
   * other then ByteString.
   *
   * Other Iteratee implementations can be used in place of this one if any
   * missing features are required.
   */
  sealed abstract class Iteratee[+A] {

    /**
     * Processes the given [[Input]], returning the resulting
     * Iteratee and the remaining Input.
     */
    final def apply(input: Input): (Iteratee[A], Input) = this match {
      case Next(f) ⇒ f(input)
      case iter    ⇒ (iter, input)
    }

    /**
     * Passes an [[EOF]] to this Iteratee and returns the
     * result if available.
     *
     * If this Iteratee is in a failure state then the Exception will be thrown.
     *
     * If this Iteratee is not well behaved (does not return a result on EOF)
     * then a "Divergent Iteratee" Exception will be thrown.
     */
    final def get: A = this(EOF)._1 match {
      case Done(value) ⇒ value
      case Next(_)     ⇒ throw new DivergentIterateeException
      case Failure(t)  ⇒ throw t
    }

    /**
     * Applies a function to the result of this Iteratee, resulting in a new
     * Iteratee. Any unused [[Input]] that is given to this
     * Iteratee will be passed to that resulting Iteratee. This is the
     * primary method of composing Iteratees together in order to process
     * an Input stream.
     */
    final def flatMap[B](f: A ⇒ Iteratee[B]): Iteratee[B] = this match {
      case Done(value)       ⇒ f(value)
      case Next(k: Chain[_]) ⇒ Next(k :+ f)
      case Next(k)           ⇒ Next(Chain(k, f))
      case f: Failure        ⇒ f
    }

    /**
     * Applies a function to transform the result of this Iteratee.
     */
    final def map[B](f: A ⇒ B): Iteratee[B] = this match {
      case Done(value)       ⇒ Done(f(value))
      case Next(k: Chain[_]) ⇒ Next(k :+ ((a: A) ⇒ Done(f(a))))
      case Next(k)           ⇒ Next(Chain(k, (a: A) ⇒ Done(f(a))))
      case f: Failure        ⇒ f
    }
  }

  /**
   * An Iteratee representing a result, usually returned by the successful
   * completion of an Iteratee. Also used to wrap any constants or
   * precalculated values that need to be composed with other Iteratees.
   */
  final case class Done[+A](result: A) extends Iteratee[A]

  /**
   * An [[Iteratee]] that still requires more input to calculate
   * it's result.
   */
  final case class Next[+A](f: Input ⇒ (Iteratee[A], Input)) extends Iteratee[A]

  /**
   * An [[Iteratee]] that represents an erronous end state.
   */
  final case class Failure(cause: Throwable) extends Iteratee[Nothing]

  //FIXME general description of what an IterateeRef is and how it is used, potentially with link to docs
  object IterateeRef {

    /**
     * Creates an [[IterateeRefSync]] containing an initial
     * [[Iteratee]].
     */
    def sync[A](initial: Iteratee[A]): IterateeRefSync[A] = new IterateeRefSync(initial)

    /**
     * Creates an empty [[IterateeRefSync]].
     */
    def sync(): IterateeRefSync[Unit] = new IterateeRefSync(Iteratee.unit)

    /**
     * Creates an [[IterateeRefAsync]] containing an initial
     * [[Iteratee]].
     */
    def async[A](initial: Iteratee[A])(implicit executor: ExecutionContext): IterateeRefAsync[A] = new IterateeRefAsync(initial)

    /**
     * Creates an empty [[IterateeRefAsync]].
     */
    def async()(implicit executor: ExecutionContext): IterateeRefAsync[Unit] = new IterateeRefAsync(Iteratee.unit)

    /**
     * A mutable Map to contain multiple IterateeRefs.
     *
     * This Map differs from the mutable Map within Scala's standard library
     * by automatically including any keys used to lookup an IterateeRef. The
     * 'refFactory' is used to provide the default value for new keys.
     */
    class Map[K, V] private (refFactory: ⇒ IterateeRef[V], underlying: mutable.Map[K, IterateeRef[V]] = mutable.Map.empty[K, IterateeRef[V]]) extends mutable.Map[K, IterateeRef[V]] {
      override def get(key: K) = Some(underlying.getOrElseUpdate(key, refFactory))
      override def iterator = underlying.iterator
      override def +=(kv: (K, IterateeRef[V])) = { underlying += kv; this }
      override def -=(key: K) = { underlying -= key; this }
      override def empty = new Map[K, V](refFactory)
    }

    //FIXME general description of what an Map is and how it is used, potentially with link to docs
    object Map {
      /**
       * Uses a factory to create the initial IterateeRef for each new key.
       */
      def apply[K, V](refFactory: ⇒ IterateeRef[V]): IterateeRef.Map[K, V] = new Map(refFactory)

      /**
       * Creates an empty [[IterateeRefSync]] for each new key.
       */
      def sync[K](): IterateeRef.Map[K, Unit] = new Map(IterateeRef.sync())

      /**
       * Creates an empty [[IterateeRefAsync]] for each new key.
       */
      def async[K]()(implicit executor: ExecutionContext): IterateeRef.Map[K, Unit] = new Map(IterateeRef.async())
    }
  }

  /**
   * A mutable reference to an Iteratee designed for use within an Actor.
   *
   * See [[IterateeRefSync]] and [[IterateeRefAsync]]
   * for details.
   */
  trait IterateeRef[A] {
    //FIXME Add docs
    def flatMap(f: A ⇒ Iteratee[A]): Unit
    //FIXME Add docs
    def map(f: A ⇒ A): Unit
    //FIXME Add docs
    def apply(input: Input): Unit
  }

  /**
   * A mutable reference to an [[Iteratee]]. Not thread safe.
   *
   * Designed for use within an [[akka.actor.Actor]].
   *
   * Includes mutable implementations of flatMap, map, and apply which
   * update the internal reference and return Unit.
   *
   * [[Input]] remaining after processing the Iteratee will
   * be stored and processed later when 'flatMap' is used next.
   */
  final class IterateeRefSync[A](initial: Iteratee[A]) extends IterateeRef[A] {
    private var _value: (Iteratee[A], Input) = (initial, Chunk.empty)
    override def flatMap(f: A ⇒ Iteratee[A]): Unit = _value = _value match {
      case (iter, chunk @ Chunk(bytes)) if bytes.nonEmpty ⇒ (iter flatMap f)(chunk)
      case (iter, input)                                  ⇒ (iter flatMap f, input)
    }
    override def map(f: A ⇒ A): Unit = _value = (_value._1 map f, _value._2)
    override def apply(input: Input): Unit = _value = _value._1(_value._2 ++ input)

    /**
     * Returns the current value of this IterateeRefSync
     */
    def value: (Iteratee[A], Input) = _value
  }

  /**
   * A mutable reference to an [[Iteratee]]. Not thread safe.
   *
   * Designed for use within an [[akka.actor.Actor]], although all actions
   * perfomed on the Iteratee are processed within a [[scala.concurrent.Future]]
   * so it is not safe to refer to the Actor's state from within this Iteratee.
   * Messages should instead be sent to the Actor in order to modify state.
   *
   * Includes mutable implementations of flatMap, map, and apply which
   * update the internal reference and return Unit.
   *
   * [[Input]] remaining after processing the Iteratee will
   * be stored and processed later when 'flatMap' is used next.
   */
  final class IterateeRefAsync[A](initial: Iteratee[A])(implicit executor: ExecutionContext) extends IterateeRef[A] {
    private var _value: Future[(Iteratee[A], Input)] = Future((initial, Chunk.empty))
    override def flatMap(f: A ⇒ Iteratee[A]): Unit = _value = _value map {
      case (iter, chunk @ Chunk(bytes)) if bytes.nonEmpty ⇒ (iter flatMap f)(chunk)
      case (iter, input)                                  ⇒ (iter flatMap f, input)
    }
    override def map(f: A ⇒ A): Unit = _value = _value map (v ⇒ (v._1 map f, v._2))
    override def apply(input: Input): Unit = _value = _value map (v ⇒ v._1(v._2 ++ input))

    /**
     * Returns a Future which will hold the future value of this IterateeRefAsync
     */
    def future: Future[(Iteratee[A], Input)] = _value
  }

  /**
   * An Iteratee that returns the ByteString prefix up until the supplied delimiter.
   * The delimiter is dropped by default, but it can be returned with the result by
   * setting 'inclusive' to be 'true'.
   */
  def takeUntil(delimiter: ByteString, inclusive: Boolean = false): Iteratee[ByteString] = {
    def step(taken: ByteString)(input: Input): (Iteratee[ByteString], Input) = input match {
      case Chunk(more) ⇒
        val bytes = taken ++ more
        val startIdx = bytes.indexOfSlice(delimiter, math.max(taken.length - delimiter.length, 0))
        if (startIdx >= 0) {
          val endIdx = startIdx + delimiter.length
          (Done(bytes take (if (inclusive) endIdx else startIdx)), Chunk(bytes drop endIdx))
        } else {
          (Next(step(bytes)), Chunk.empty)
        }
      case EOF              ⇒ (Failure(new EOFException("Unexpected EOF")), EOF)
      case e @ Error(cause) ⇒ (Failure(cause), e)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Iteratee that will collect bytes as long as a predicate is true.
   */
  def takeWhile(p: (Byte) ⇒ Boolean): Iteratee[ByteString] = {
    def step(taken: ByteString)(input: Input): (Iteratee[ByteString], Input) = input match {
      case Chunk(more) ⇒
        val (found, rest) = more span p
        if (rest.isEmpty)
          (Next(step(taken ++ found)), Chunk.empty)
        else
          (Done(taken ++ found), Chunk(rest))
      case EOF              ⇒ (Failure(new EOFException("Unexpected EOF")), EOF)
      case e @ Error(cause) ⇒ (Failure(cause), e)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Iteratee that returns a ByteString of the requested length.
   */
  def take(length: Int): Iteratee[ByteString] = {
    def step(taken: ByteString)(input: Input): (Iteratee[ByteString], Input) = input match {
      case Chunk(more) ⇒
        val bytes = taken ++ more
        if (bytes.length >= length)
          (Done(bytes.take(length)), Chunk(bytes.drop(length)))
        else
          (Next(step(bytes)), Chunk.empty)
      case EOF              ⇒ (Failure(new EOFException("Unexpected EOF")), EOF)
      case e @ Error(cause) ⇒ (Failure(cause), e)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Iteratee that ignores the specified number of bytes.
   */
  def drop(length: Int): Iteratee[Unit] = {
    def step(left: Int)(input: Input): (Iteratee[Unit], Input) = input match {
      case Chunk(more) ⇒
        if (left > more.length)
          (Next(step(left - more.length)), Chunk.empty)
        else
          (Done(()), Chunk(more drop left))
      case EOF              ⇒ (Failure(new EOFException("Unexpected EOF")), EOF)
      case e @ Error(cause) ⇒ (Failure(cause), e)
    }

    Next(step(length))
  }

  /**
   * An Iteratee that returns the remaining ByteString until an EOF is given.
   */
  val takeAll: Iteratee[ByteString] = {
    def step(taken: ByteString)(input: Input): (Iteratee[ByteString], Input) = input match {
      case Chunk(more) ⇒
        val bytes = taken ++ more
        (Next(step(bytes)), Chunk.empty)
      case EOF              ⇒ (Done(taken), EOF)
      case e @ Error(cause) ⇒ (Failure(cause), e)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Iteratee that returns any input it receives
   */
  val takeAny: Iteratee[ByteString] = Next {
    case Chunk(bytes) if bytes.nonEmpty ⇒ (Done(bytes), Chunk.empty)
    case Chunk(bytes)                   ⇒ (takeAny, Chunk.empty)
    case EOF                            ⇒ (Done(ByteString.empty), EOF)
    case e @ Error(cause)               ⇒ (Failure(cause), e)
  }

  /**
   * An Iteratee that creates a list made up of the results of an Iteratee.
   */
  def takeList[A](length: Int)(iter: Iteratee[A]): Iteratee[List[A]] = {
    def step(left: Int, list: List[A]): Iteratee[List[A]] =
      if (left == 0) Done(list.reverse)
      else iter flatMap (a ⇒ step(left - 1, a :: list))

    step(length, Nil)
  }

  /**
   * An Iteratee that returns a [[akka.util.ByteString]] of the request length,
   * but does not consume the Input.
   */
  def peek(length: Int): Iteratee[ByteString] = {
    def step(taken: ByteString)(input: Input): (Iteratee[ByteString], Input) = input match {
      case Chunk(more) ⇒
        val bytes = taken ++ more
        if (bytes.length >= length)
          (Done(bytes.take(length)), Chunk(bytes))
        else
          (Next(step(bytes)), Chunk.empty)
      case EOF              ⇒ (Done(taken), EOF)
      case e @ Error(cause) ⇒ (Failure(cause), e)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Iteratee that continually repeats an Iteratee.
   */
  def repeat[T](iter: Iteratee[T]): Iteratee[T] = iter flatMap (_ ⇒ repeat(iter))

  /**
   * An Iteratee that applies an Iteratee to each element of a Traversable
   * and finally returning a single Iteratee containing a Traversable of the results.
   */
  def traverse[A, B, M[A] <: Traversable[A]](in: M[A])(f: A ⇒ Iteratee[B])(implicit cbf: CanBuildFrom[M[A], B, M[B]]): Iteratee[M[B]] =
    fold(cbf(in), in)((b, a) ⇒ f(a) map (b += _)) map (_.result)

  /**
   * An Iteratee that folds over a Traversable by applying a function that
   * returns an Iteratee.
   */
  def fold[A, B, M[A] <: Traversable[A]](initial: B, in: M[A])(f: (B, A) ⇒ Iteratee[B]): Iteratee[B] =
    (Iteratee(initial) /: in)((ib, a) ⇒ ib flatMap (b ⇒ f(b, a)))

  // private api

  private object Chain {
    def apply[A](f: Input ⇒ (Iteratee[A], Input)) = new Chain[A](f, Nil, Nil)
    def apply[A, B](f: Input ⇒ (Iteratee[A], Input), k: A ⇒ Iteratee[B]) = new Chain[B](f, List(k.asInstanceOf[Any ⇒ Iteratee[Any]]), Nil)
  }

  /**
   * A function 'ByteString => Iteratee[A]' that composes with 'A => Iteratee[B]' functions
   * in a stack-friendly manner.
   *
   * For internal use within Iteratee.
   */
  private final case class Chain[A] private (cur: Input ⇒ (Iteratee[Any], Input), queueOut: List[Any ⇒ Iteratee[Any]], queueIn: List[Any ⇒ Iteratee[Any]]) extends (Input ⇒ (Iteratee[A], Input)) {

    def :+[B](f: A ⇒ Iteratee[B]) = new Chain[B](cur, queueOut, f.asInstanceOf[Any ⇒ Iteratee[Any]] :: queueIn)

    def apply(input: Input): (Iteratee[A], Input) = {
      @tailrec
      def run(result: (Iteratee[Any], Input), queueOut: List[Any ⇒ Iteratee[Any]], queueIn: List[Any ⇒ Iteratee[Any]]): (Iteratee[Any], Input) = {
        if (queueOut.isEmpty) {
          if (queueIn.isEmpty) result
          else run(result, queueIn.reverse, Nil)
        } else result match {
          case (Done(value), rest) ⇒
            queueOut.head(value) match {
              case Next(f) ⇒ run(f(rest), queueOut.tail, queueIn)
              case iter    ⇒ run((iter, rest), queueOut.tail, queueIn)
            }
          case (Next(f), rest) ⇒ (Next(new Chain(f, queueOut, queueIn)), rest)
          case _               ⇒ result
        }
      }
      run(cur(input), queueOut, queueIn).asInstanceOf[(Iteratee[A], Input)]
    }
  }

}
