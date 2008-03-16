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

trait TestScanner extends Rules {
  type S

  def input(string : String) : S
  
  def remaining(s : S) : String
  
  def checkSuccess[A](input : String, result : Result[S, A, Nothing], expected : A) {
    result match {
      case Success(restm, actual) if actual == expected => ()
      case actual => fail(input, actual, expected, "")
    }
  }
  
  def check[A](input : String, actual : Result[S, A, Nothing], expected : A, rest : String) {
    actual match {
      case Success(es, ea) => if (ea != expected && !remaining(es).equals(rest)) 
        fail(input, actual, expected, rest)
      case _ => fail(input, actual, expected, rest)
    }
  }
  
  def fail[A](input : String, actual : Result[S, A, Nothing], expected : A, rest : String) {
    actual match {
      case Success(s, result) =>  error ("Input: " + input + 
        "\nExpected success: " + expected + 
        "\nWith remaining input: \"" + rest + "\"" +
        "\n\nActual success value: " + result +
        "\nWith remaining input: \"" + remaining(s) + "\"") 
      case _ => error ("Input: " + input + 
        "\nExpected success: " + expected + 
          "\nWith remaining input: \"" + rest + "\"" +
          "\n\nActual result: " + actual)
    }
  }
  
  def checkFailure[A](rule : Rule[S, S, A, Nothing])(inputs : String *) {
    for (string <- inputs) {
      rule(input(string)) match {
        case Failure => ()
        case actual => error ("Input: " + string + 
          "\nExpected Failure" + 
          "\nActual result: " + actual)
      }
    }
  }
  
  def checkRule[A](rule : Rule[S, S, A, Nothing])(expect : (String, A) *) {
    for ((string, result) <- expect) {
      checkSuccess(string, rule(input(string)), result)
    }
  }
  
  def checkRuleWithRest[A](rule : Rule[S, S, A, Nothing])(expect : ((String, A), String) *) {
    for (((string, result), rest) <- expect) {
      check(string, rule(input(string)), result, rest)
    }
  }
}

