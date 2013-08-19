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
   * To keep this implementation simple it has no support for Enumerator or ByteString types
   * other then ByteString.
   *
   * Other Iteratee implementations can be used in place of this one if any
   * missing features are required.
   */
  sealed abstract class Iteratee[+A] {

    /**
     * Processes the given [[ByteString]], returning the resulting
     * Iteratee and the remaining ByteString.
     */
    final def apply(input: ByteString): (Iteratee[A], ByteString) = this match {
      case Next(f) ⇒ f(input)
      case iter    ⇒ (iter, input)
    }

    /**
     * Applies a function to the result of this Iteratee, resulting in a new
     * Iteratee. Any unused [[ByteString]] that is given to this
     * Iteratee will be passed to that resulting Iteratee. This is the
     * primary method of composing Iteratees together in order to process
     * an ByteString stream.
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
  final case class Next[+A](f: ByteString ⇒ (Iteratee[A], ByteString)) extends Iteratee[A]

  /**
   * An [[Iteratee]] that represents an erronous end state.
   */
  final case class Failure(cause: Throwable) extends Iteratee[Nothing]


  /**
   * A mutable reference to an [[Iteratee]]. Not thread safe.
   *
   * Includes mutable implementations of flatMap, map, and apply which
   * update the internal reference and return Unit.
   *
   * [[ByteString]] remaining after processing the Iteratee will
   * be stored and processed later when 'flatMap' is used next.
   */
  case class IterateeRef[A](initial: Iteratee[A]) {
    private var _value: (Iteratee[A], ByteString) = (initial, ByteString.empty)
    def flatMap(f: A ⇒ Iteratee[A]): Unit = _value = _value match {
      case (iter, bytes) if bytes.nonEmpty ⇒ (iter flatMap f)(bytes)
    }
    def map(f: A ⇒ A): Unit = _value = (_value._1 map f, _value._2)
    def apply(input: ByteString): Unit = _value = _value._1(_value._2 ++ input)

    /**
     * Returns the current value of this IterateeRefSync
     */
    def value: (Iteratee[A], ByteString) = _value
  }


  /**
   * An Iteratee that returns the ByteString prefix up until the supplied delimiter.
   * The delimiter is dropped by default, but it can be returned with the result by
   * setting 'inclusive' to be 'true'.
   */
  def takeUntil(delimiter: ByteString, inclusive: Boolean = false): Iteratee[ByteString] = {
    def step(taken: ByteString)(input: ByteString): (Iteratee[ByteString], ByteString) = {
      val bytes = taken ++ input
      val startIdx = bytes.indexOfSlice(delimiter, math.max(taken.length - delimiter.length, 0))
      if (startIdx >= 0) {
        val endIdx = startIdx + delimiter.length
        (Done(bytes take (if (inclusive) endIdx else startIdx)), bytes drop endIdx)
      } else {
        (Next(step(bytes)), ByteString.empty)
      }
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Iteratee that will collect bytes as long as a predicate is true.
   */
  def takeWhile(p: (Byte) ⇒ Boolean): Iteratee[ByteString] = {
    def step(taken: ByteString)(input: ByteString): (Iteratee[ByteString], ByteString) = {
      val (found, rest) = input span p
      if (rest.isEmpty)
        (Next(step(taken ++ found)), ByteString.empty)
      else
        (Done(taken ++ found), rest)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Iteratee that returns a ByteString of the requested length.
   */
  def take(length: Int): Iteratee[ByteString] = {
    def step(taken: ByteString)(input: ByteString): (Iteratee[ByteString], ByteString) = {
      val bytes = taken ++ input
      if (bytes.length >= length)
        (Done(bytes.take(length)), bytes.drop(length))
      else
        (Next(step(bytes)), ByteString.empty)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Iteratee that ignores the specified number of bytes.
   */
  def drop(length: Int): Iteratee[Unit] = {
    def step(left: Int)(input: ByteString): (Iteratee[Unit], ByteString) = {
      if (left > input.length)
        (Next(step(left - input.length)), ByteString.empty)
      else
        (Done(()), input drop left)
    }

    Next(step(length))
  }

  /**
   * An Iteratee that returns the remaining ByteString until an EOF is given.
   */
  val takeAll: Iteratee[ByteString] = {
    def step(taken: ByteString)(input: ByteString): (Iteratee[ByteString], ByteString) = {
      val bytes = taken ++ input
      (Next(step(bytes)), ByteString.empty)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Iteratee that returns any input it receives
   */
  val takeAny: Iteratee[ByteString] = Next {
    case bytes if bytes.nonEmpty ⇒ (Done(bytes), ByteString.empty)
    case bytes                   ⇒ (takeAny, ByteString.empty)
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
   * but does not consume the ByteString.
   */
  def peek(length: Int): Iteratee[ByteString] = {
    def step(taken: ByteString)(input: ByteString): (Iteratee[ByteString], ByteString) = {
      val bytes = taken ++ input
      if (bytes.length >= length)
        (Done(bytes.take(length)), bytes)
      else
        (Next(step(bytes)), ByteString.empty)
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
    def apply[A](f: ByteString ⇒ (Iteratee[A], ByteString)) = new Chain[A](f, Nil, Nil)
    def apply[A, B](f: ByteString ⇒ (Iteratee[A], ByteString), k: A ⇒ Iteratee[B]) = new Chain[B](f, List(k.asInstanceOf[Any ⇒ Iteratee[Any]]), Nil)
  }

  /**
   * A function 'ByteString => Iteratee[A]' that composes with 'A => Iteratee[B]' functions
   * in a stack-friendly manner.
   *
   * For internal use within Iteratee.
   */
  private final case class Chain[A] private (cur: ByteString ⇒ (Iteratee[Any], ByteString), queueOut: List[Any ⇒ Iteratee[Any]], queueIn: List[Any ⇒ Iteratee[Any]]) extends (ByteString ⇒ (Iteratee[A], ByteString)) {

    def :+[B](f: A ⇒ Iteratee[B]) = new Chain[B](cur, queueOut, f.asInstanceOf[Any ⇒ Iteratee[Any]] :: queueIn)

    def apply(input: ByteString): (Iteratee[A], ByteString) = {
      @tailrec
      def run(result: (Iteratee[Any], ByteString), queueOut: List[Any ⇒ Iteratee[Any]], queueIn: List[Any ⇒ Iteratee[Any]]): (Iteratee[Any], ByteString) = {
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
      run(cur(input), queueOut, queueIn).asInstanceOf[(Iteratee[A], ByteString)]
    }
  }

}
