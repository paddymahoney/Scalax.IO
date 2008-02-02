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

trait TestScanner extends Scanner with Application {

  def input(string : String) : S
  
  def checkSuccess[A](input : String, result : Result[(A, S)], expected : A) {
    result match {
      case Success((actual, rest)) if actual == expected => ()
      case actual => fail(input, actual, expected, "")
    }
  }
  
  def check[A](input : String, actual : Result[(A, S)], expected : A, rest : String) {
    actual match {
      case Success((ea, es)) => if (ea != expected && !es.mkString("").equals(rest)) 
        fail(input, actual, expected, rest)
      case _ => fail(input, actual, expected, rest)
    }
  }
  
  def fail[A](input : String, actual : Result[(A, S)], expected : A, rest : String) {
    actual match {
      case Success((result, s)) =>  error ("Input: " + input + 
        "\nExpected success: " + expected + 
        "\nWith remaining input: \"" + rest + "\"" +
        "\n\nActual success value: " + result +
        "\nWith remaining input: \"" + s.mkString("") + "\"") 
      case _ => error ("Input: " + input + 
        "\nExpected success: " + expected + 
          "\nWith remaining input: \"" + rest + "\"" +
          "\n\nActual result: " + actual)
    }
  }
  
  def checkFailure[A](rule : Rule[A])(inputs : String *) {
    for (string <- inputs) {
      val actual = rule(input(string))
      if (actual != Failure) error ("Input: " + string + 
        "\nExpected Failure" + 
        "\nActual result: " + actual)
    }
  }
  
  def checkRule[A](rule : Rule[A])(expect : (String, A) *) {
    for ((string, result) <- expect) {
      checkSuccess(string, rule(input(string)), result)
    }
  }
  
  def checkRuleWithRest[A](rule : Rule[A])(expect : ((String, A), String) *) {
    for (((string, result), rest) <- expect) {
      check(string, rule(input(string)), result, rest)
    }
  }
}

