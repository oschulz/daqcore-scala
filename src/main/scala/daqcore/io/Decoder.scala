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

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom

import akka.util.ByteString

import java.io.EOFException


/**
 * A basic Decoder implementation of Oleg's Decoder (http://okmij.org/ftp/Streams.html).
 * To keep this implementation simple it has no support for Enumerator or ByteString types
 * other then ByteString.
 *
 * Other Decoder implementations can be used in place of this one if any
 * missing features are required.
 */
sealed abstract class Decoder[+A] {
  import Decoder.{Done, Next, Failure, Chain}

  /**
   * Processes the given [[ByteString]], returning the resulting
   * Decoder and the remaining ByteString.
   */
  final def apply(input: ByteString): (Decoder[A], ByteString) = this match {
    case Next(f) ⇒ f(input)
    case iter    ⇒ (iter, input)
  }

  /**
   * Applies a function to the result of this Decoder, resulting in a new
   * Decoder. Any unused [[ByteString]] that is given to this
   * Decoder will be passed to that resulting Decoder. This is the
   * primary method of composing Decoders together in order to process
   * an ByteString stream.
   */
  final def flatMap[B](f: A ⇒ Decoder[B]): Decoder[B] = this match {
    case Done(value)       ⇒ f(value)
    case Next(k: Chain[_]) ⇒ Next(k :+ f)
    case Next(k)           ⇒ Next(Chain(k, f))
    case f: Failure        ⇒ f
  }

  /**
   * Applies a function to transform the result of this Decoder.
   */
  final def map[B](f: A ⇒ B): Decoder[B] = this match {
    case Done(value)       ⇒ Done(f(value))
    case Next(k: Chain[_]) ⇒ Next(k :+ ((a: A) ⇒ Done(f(a))))
    case Next(k)           ⇒ Next(Chain(k, (a: A) ⇒ Done(f(a))))
    case f: Failure        ⇒ f
  }
}


object Decoder {
  /**
   * Wrap the provided value within a [[Done]]
   * [[Decoder]]. This is a helper for cases where the type should be
   * inferred as an Decoder and not as a Done.
   */
  def apply[A](value: A): Decoder[A] = Done(value)

  /**
   * Returns Decoder.unit
   */
  def apply(): Decoder[Unit] = unit

  /**
   * The single value representing Done(())
   */
  val unit: Decoder[Unit] = Done(())

  
  /**
   * An Decoder representing a result, usually returned by the successful
   * completion of an Decoder. Also used to wrap any constants or
   * precalculated values that need to be composed with other Decoders.
   */
  final case class Done[+A](result: A) extends Decoder[A]

  /**
   * An [[Decoder]] that still requires more input to calculate
   * it's result.
   */
  final case class Next[+A](f: ByteString ⇒ (Decoder[A], ByteString)) extends Decoder[A]

  /**
   * An [[Decoder]] that represents an erronous end state.
   */
  final case class Failure(cause: Throwable) extends Decoder[Nothing]


  /**
   * An Decoder that returns the ByteString prefix up until the supplied delimiter.
   * The delimiter is dropped by default, but it can be returned with the result by
   * setting 'inclusive' to be 'true'.
   */
  def takeUntil(delimiter: ByteString, inclusive: Boolean = false): Decoder[ByteString] = {
    def step(taken: ByteString)(input: ByteString): (Decoder[ByteString], ByteString) = {
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
   * An Decoder that will collect bytes as long as a predicate is true.
   */
  def takeWhile(p: (Byte) ⇒ Boolean): Decoder[ByteString] = {
    def step(taken: ByteString)(input: ByteString): (Decoder[ByteString], ByteString) = {
      val (found, rest) = input span p
      if (rest.isEmpty)
        (Next(step(taken ++ found)), ByteString.empty)
      else
        (Done(taken ++ found), rest)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Decoder that returns a ByteString of the requested length.
   */
  def take(length: Int): Decoder[ByteString] = {
    def step(taken: ByteString)(input: ByteString): (Decoder[ByteString], ByteString) = {
      val bytes = taken ++ input
      if (bytes.length >= length)
        (Done(bytes.take(length)), bytes.drop(length))
      else
        (Next(step(bytes)), ByteString.empty)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Decoder that ignores the specified number of bytes.
   */
  def drop(length: Int): Decoder[Unit] = {
    def step(left: Int)(input: ByteString): (Decoder[Unit], ByteString) = {
      if (left > input.length)
        (Next(step(left - input.length)), ByteString.empty)
      else
        (Done(()), input drop left)
    }

    Next(step(length))
  }

  /**
   * An Decoder that returns the remaining ByteString until an EOF is given.
   */
  val takeAll: Decoder[ByteString] = {
    def step(taken: ByteString)(input: ByteString): (Decoder[ByteString], ByteString) = {
      val bytes = taken ++ input
      (Next(step(bytes)), ByteString.empty)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Decoder that returns any input it receives
   */
  val takeAny: Decoder[ByteString] = Next {
    case bytes if bytes.nonEmpty ⇒ (Done(bytes), ByteString.empty)
    case bytes                   ⇒ (takeAny, ByteString.empty)
  }

  /**
   * An Decoder that creates a list made up of the results of an Decoder.
   */
  def takeList[A](length: Int)(iter: Decoder[A]): Decoder[List[A]] = {
    def step(left: Int, list: List[A]): Decoder[List[A]] =
      if (left == 0) Done(list.reverse)
      else iter flatMap (a ⇒ step(left - 1, a :: list))

    step(length, Nil)
  }

  /**
   * An Decoder that returns a [[akka.util.ByteString]] of the request length,
   * but does not consume the ByteString.
   */
  def peek(length: Int): Decoder[ByteString] = {
    def step(taken: ByteString)(input: ByteString): (Decoder[ByteString], ByteString) = {
      val bytes = taken ++ input
      if (bytes.length >= length)
        (Done(bytes.take(length)), bytes)
      else
        (Next(step(bytes)), ByteString.empty)
    }

    Next(step(ByteString.empty))
  }

  /**
   * An Decoder that continually repeats an Decoder.
   */
  def repeat[T](iter: Decoder[T]): Decoder[T] = iter flatMap (_ ⇒ repeat(iter))

  /**
   * An Decoder that applies an Decoder to each element of a Traversable
   * and finally returning a single Decoder containing a Traversable of the results.
   */
  def traverse[A, B, M[A] <: Traversable[A]](in: M[A])(f: A ⇒ Decoder[B])(implicit cbf: CanBuildFrom[M[A], B, M[B]]): Decoder[M[B]] =
    fold(cbf(in), in)((b, a) ⇒ f(a) map (b += _)) map (_.result)

  /**
   * An Decoder that folds over a Traversable by applying a function that
   * returns an Decoder.
   */
  def fold[A, B, M[A] <: Traversable[A]](initial: B, in: M[A])(f: (B, A) ⇒ Decoder[B]): Decoder[B] =
    (Decoder(initial) /: in)((ib, a) ⇒ ib flatMap (b ⇒ f(b, a)))

  // private api

  private object Chain {
    def apply[A](f: ByteString ⇒ (Decoder[A], ByteString)) = new Chain[A](f, Nil, Nil)
    def apply[A, B](f: ByteString ⇒ (Decoder[A], ByteString), k: A ⇒ Decoder[B]) = new Chain[B](f, List(k.asInstanceOf[Any ⇒ Decoder[Any]]), Nil)
  }

  /**
   * A function 'ByteString => Decoder[A]' that composes with 'A => Decoder[B]' functions
   * in a stack-friendly manner.
   *
   * For internal use within Decoder.
   */
  private final case class Chain[A] private (cur: ByteString ⇒ (Decoder[Any], ByteString), queueOut: List[Any ⇒ Decoder[Any]], queueIn: List[Any ⇒ Decoder[Any]]) extends (ByteString ⇒ (Decoder[A], ByteString)) {

    def :+[B](f: A ⇒ Decoder[B]) = new Chain[B](cur, queueOut, f.asInstanceOf[Any ⇒ Decoder[Any]] :: queueIn)

    def apply(input: ByteString): (Decoder[A], ByteString) = {
      @tailrec
      def run(result: (Decoder[Any], ByteString), queueOut: List[Any ⇒ Decoder[Any]], queueIn: List[Any ⇒ Decoder[Any]]): (Decoder[Any], ByteString) = {
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
      run(cur(input), queueOut, queueIn).asInstanceOf[(Decoder[A], ByteString)]
    }
  }

}
