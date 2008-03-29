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

package scalax.rules.syntax.test

object TestScalaScanner extends ScalaScanner {
  type S = String
    
  lazy val item = from[String] { 
    case "" => Failure
    case s => Success(s.substring(1), s.charAt(0)) 
  }
  
  DefaultMemoisable.debug = true
  
  def newlineAllowed = failure
  def pos = unit { () => 0 }
  
  def checkSuccess[A](rule : Rule[A, Any])(expect : (String, A) *) {
    for ((string, result) <- expect) rule(string) match {
      case success @ Success("", `result`) => println("OK: " + string + " -> " + success)
      case other => println("FAILED: " + string + " -> " + other)
    }
  }
  
  def checkRule(rule : Rule[Any, Any])(expect : (String, Result[S, Any, Any]) *) {
    for ((input, result) <- expect) rule(input) match {
      case `result` => println("OK: " + input + " -> " + result)
      case other => println("FAILED: " + input + " -> " + other)
    }
  }
  

  
  def main(args : Array[String]) {
    
    checkRule(singleQuotedString)(
        "\"hello\"" -> Success("", "hello"),
        "\"hello" -> Error(SyntaxError("Unterminated string"))
    )

  }
}

