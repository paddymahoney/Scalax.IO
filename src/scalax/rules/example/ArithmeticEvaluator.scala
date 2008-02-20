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

package scalax.rules.example

trait ArithmeticEvaluator extends IncrementalScanner {
  type S = DefaultIncrementalInput
  
  lazy val expr : Rule[Int] = term ~*~ (op('+', _ + _) | op('-', _ - _))
  lazy val term : Rule[Int] = factor ~*~ (op('*', _ * _) | op('/', _ / _))
  lazy val factor : Rule[Int] = memo("factor", trim(number | '(' -~ expr ~- ')'))
  lazy val number = (('0' to '9')+) ^^ toString ^^ (_ toInt)
  
  private def op(r : Rule[Any], f : (Int, Int) => Int) : Rule[(Int, Int) => Int] = r -^ f

  def evaluate = expect(expr ~- (!item | Rule.error("Invalid expression")))
}
  
object ExampleUsage extends ArithmeticEvaluator with Application {
  type Context = DefaultIncrementalInput
  val input = new DefaultIncrementalInput
  
  DefaultIncrementalInput.debug = true
  
  // set up initial text and evaluate
  input.edit(0, 0, "7 + 5 * (5+ 6 / 2 - 1)")
  println(evaluate(input))
  
   // change to "7 + (5 + 1) * (5+ 6 / 2 - 1)"
  input.edit(4, 1, "(5 + 1)")
  println(evaluate(input))
  
   // change to "(5 + 1) * (5+ 6 / 2 - 1)"
  input.edit(0, 4, "")
  println(evaluate(input))
}
