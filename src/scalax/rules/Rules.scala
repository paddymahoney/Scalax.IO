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

package scalax.rules

/** Defines Rules that apply to a particular Context.
 * 
 * The result may be:
 * - Success, with a resulting context and a value of some type.  Several successful rules may be applied in sequence and their values combined.
 * - Failure. A failure may result in some alternative rule being applied.
 * - Error.  No further rules should be attempted.
 *
 * @requires S the context to which rules apply.
 *
 * @author Andrew Foggin
 
 * Inspired by the Scala parser combinator.
 */
trait Rules extends RuleFactory { //extends MonadsWithZero with StateReader {
  type S
  type Rule[+A] = rules.Rule[S, S, A, Any, Nothing]
  type Result[A] = rules.Result[S, A, Any, Nothing]
  
  implicit def rule[A](f : S => Result[A]) : Rule[A] = createRule(f)
  
  def get = rule[S] { s => Success(s, s) }
  def read[A](f : S => A) = rule[A] { s => Success(s, f(s)) }
  def set(s : => S) = rule { oldS => Success(s, oldS) }
  def update(f : S => S) = rule { s => Success(f(s), s) }
  
  /** Creates a Rule that always succeeds with the specified value. */
  def success[A](a : A) = rule[A] { s => Success(s, a) }
    
  val failure = rule[Nothing] { s => Failure() }
  
  lazy val nil = success(Nil)
  lazy val none = success(None)
    
  /** Primitive rule that always suceeds and returns the context with which it is called. */
  lazy val context = get
    
  /** Create a rule that suceeds if f(context) is true.  The value returned is the context. */
  def predicate(f : S => Boolean) = for (ctx <- context if f(ctx)) yield ctx

  /** Converts a rule into a function that throws an Exception on failure.
   */
  def expect[A](rule : Rule[A]) : S => A = (context) => rule(context) match {
    case Success(_, a) => a
    case Failure(x) => throw new RuntimeException("Unexpected failure: " + x)
    case Error(x) => throw new RuntimeException("Unexpected failure: " + x)
  }
    
  def select[A](rules : Collection[Rule[A]]) : Rule[A] = rules.reduceLeft[Rule[A]](_ | _)
  
  

}

