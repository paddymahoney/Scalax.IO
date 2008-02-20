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

package scalax.rules.syntax

case class ParserState(multipleStatementsAllowed : Boolean, lastTokenCanEndStatement : Boolean) 

class ScalaInput[T <: Input[Char, T] with Memoisable[T]](val input : T, val state : ParserState) 
    extends Input[Char, ScalaInput[T]] with Memoisable[ScalaInput[T]]  {

  def this(input : T) = this(input, ParserState(true, false))
  
  def index = input.index
  
  def next = input.next match {
    case Success(input, ch) => Success(new ScalaInput(input, state), ch)
    case _ => Failure(())
  }
  
  def state_=(state : ParserState) = new ScalaInput(input, state)

  def multipleStatementsAllowed = state.multipleStatementsAllowed
  def multipleStatementsAllowed_=(value : Boolean) = state_=(ParserState(value, lastTokenCanEndStatement))

  def lastTokenCanEndStatement = state.lastTokenCanEndStatement
  def lastTokenCanEndStatement_=(value : Boolean) = state_=(ParserState(multipleStatementsAllowed, value))

  def memo[B](key : AnyRef, f : ScalaInput[T] => Result[ScalaInput[T], B, Any]) : Result[ScalaInput[T], B, Any] = {
    // Uses the underlying input's memo function by augmenting both the key and the result with the parser state
    val result = input.memo((key, state), input => f(this) match {
      case Success(context, b) => Success(context.input, (b, context))
      case _ => Failure(())
    })
    result match {
      case Success(input, (b, context)) => Success(context, b)
      case _ => Failure(())
    }
  }

  override def toString = state + input.toString
}
