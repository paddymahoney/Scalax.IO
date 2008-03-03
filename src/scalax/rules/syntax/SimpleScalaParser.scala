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

package scalax.rules.syntax;


class SimpleScalaParser extends MemoisableRules with ScalaParser {
  type S = Input#State
  
  //DefaultMemoisable.debug = true
  
  case class Input(chars : Seq[Char], index : Int) {
    
    lazy val nextInput = Input(chars, index + 1)
    
    val state = Map(
        (true, true) -> State(true, true),
        (true, false) -> State(true, false),
        (false, true) -> State(false, true),
        (false, false) -> State(false, false)
    )
    
    def apply(multiple : Boolean, canEnd : Boolean) = state(multiple, canEnd)
    
    case class State(multiple : Boolean, canEnd : Boolean) extends DefaultMemoisable {
      def next = if (index < chars.length) 
        Success(nextInput(multiple, canEnd), chars(index)) 
        else Failure("End of input")
        
      def apply(multiple : Boolean, canEnd : Boolean) = state(multiple, canEnd)
      def index = Input.this.index
      def chars = Input.this.chars
    }
  }
  
  def input(chars : Seq[Char]) = Input(chars, 0)(true, false)
  
  val multipleStatementsAllowed = predicate(_.multiple)
  val lastTokenCanEndStatement = predicate(_.canEnd)

  def multiple(allow : Boolean) = rule { input => Success(input(allow, input.canEnd), input.multiple) }
  def lastTokenCanEndStatement(value : Boolean) = rule { input => Success(input(input.multiple, value), input.canEnd) }
    
  val position = read { input => () => input.index } //context ^^ { ctx => () => ctx.index }
  
  def item = rule { _ next }

}

