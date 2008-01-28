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
/** 
 * Throw this exception or a subclass to indicate a failure without alternatives
 */
class RuleException[Context](val context : Context, message : String) extends Exception(message)

object Result extends MonadsWithZero {
  type M[+A] = Result[A]
  
  def unit[A](a : => A) = Success(a)
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
