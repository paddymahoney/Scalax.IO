// -----------------------------------------------------------------------------
//
//  Scalax - The Scala Community Library
//  Copyright (c) 2005-8 The Scalax Project. All rights reserved.
//
//  The primary distribution site is http://scalax.scalaforge.org/
//
//  This software is released under the terms of the Revised BSD License.
//  There is NO WARRANTY.  See the file LICENSE for the full text.
//
// -----------------------------------------------------------------------------

package scalax.rules;

/** Represents the combined value of two rules applied in sequence.
 *
 * @see the Scala parser combinator
 */
case class ~[+A, +B](_1 : A, _2 : B)
  
sealed abstract class Result[+Out, +A, +X] extends Functor[A] with OrElse[A] {
  type M[+B] = Result[_, B, _]
}

case class Success[+Out, +A](out : Out, value : A) extends Result[Out, A, Nothing] {
  def map[B](f : A => B) = Success(out, f(value))
  def orElse[B >: A](other : => M[B]) = this
}

case object Failure extends Result[Nothing, Nothing, Nothing] {
  def map[B](f : Nothing => B) = this
  def orElse[B](other : => M[B]) = other
}

case class Error[+X](x : X) extends Result[Nothing, Nothing, X] {
  def map[B](f : Nothing => B) = this
  def orElse[B](other : => M[B]) = this
}
