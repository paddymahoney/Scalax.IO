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
   

sealed abstract class Result[+Out, +A, +X] {
  
}

case class Success[+Out, +A](out : Out, value : A) extends Result[Out, A, Nothing] {
  
}

case class Failure[+X](x : X) extends Result[Nothing, Nothing, X]

case class Error[+X](override val x : X) extends Failure(x)

//object Failure {
//  def apply : Failure[Unit] = Failure((), true)
//}
/*
object Result extends MonadsWithZero {
  type Fun[+A] = Result[A]
  
  override def unit[A](a : => A) = Success(a)
  override def zero = Failure
}
  
/** 
 * A Result is either Success or Failure.
 *
 * @author Andrew Foggin
 */
sealed abstract class Result[+A] extends Result.MonadWithZero[A] with Result.OrElse[A]
      
/** 
 * Result of a rule that was successfully applied.  
 */
case class Success[+A](value : A) extends Result[A] {
  def flatMap[B](f : A => Result[B]) = f(value)
  def orElse[B >: A](other : => Result[B]) = this
}
      
/** 
 * Result of a rule that could not be applied.
 */
case object Failure extends Result[Nothing] with Result.ZeroOrElse
*/