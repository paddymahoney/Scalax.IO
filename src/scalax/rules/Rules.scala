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

trait Name {
  def name : String
  override def toString = name
}

trait RuleFactory {
  implicit def rule[In, Out, A, X](f : In => Result[Out, A, X]) : Rule[In, Out, A, X] = new DefaultRule(f)

  implicit def seqRule[In, A, X](rule : Rule[In, In, A, X]) : SeqRule[In, A, X] = new SeqRule(rule)
  
  def from[In] = new {
    def apply[Out, A, X, Err](f : In => Result[Out, A, X]) = rule(f)
    def error = apply { in => Error(in) }
    def unit[A](a : A) = apply { in => Success(in, a) }
    def get = apply { in => Success(in, in) }
    def read[A](f : In => A) = apply { in => Success(in, f(in)) }
  }
  
  def success[Out, A](out : Out, a : A) = rule { in : Any => Success(out, a) }
  def failure = rule { in : Any => Failure }
  def error[X](err : X) = { in : Any => Error(err) }

  def ruleWithName[In, Out, A, X](_name : String, f : In => Result[Out, A, X]) : Rule[In, Out, A, X] with Name = 
    new DefaultRule(f) with Name {
      val name = _name
    }

  class DefaultRule[In, Out, A, X](f : In => Result[Out, A, X]) extends Rule[In, Out, A, X] {
    val factory = RuleFactory.this
    def apply(in : In) = f(in)
  }
}

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
trait StateRules extends RuleFactory {
  type S
  type Rule[+A] = rules.Rule[S, S, A, Nothing]
  type Result[A] = rules.Result[S, A, Nothing]
  
  def get = from[S] { s => Success(s, s) }
  def read[A](f : S => A) = from[S] read(f)
  def set(s : => S) = from[S] { oldS => Success(s, oldS) }
  def update(f : S => S) = from[S] { s => Success(f(s), s) }
  
  /** Creates a Rule that always succeeds with the specified value. */
  def success[A](a : A) = from[S] unit(a)
    
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
    case Failure => throw new RuntimeException("Unexpected failure")
    case Error(x) => throw new RuntimeException("Unexpected failure: " + x)
  }
    
  def select[A](rules : Collection[Rule[A]]) : Rule[A] = rules.reduceLeft[Rule[A]](_ | _)
}

