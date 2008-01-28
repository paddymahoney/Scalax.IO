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


/**
 * A Parser is used to define rules that operate on sequential input
 */
trait Parser[A] extends Rules {
  type S <: Input[A, S]

  /** Succeeds with the first element of the input unless input is empty. */
  val item = rule[A] { input => input.next }

  implicit def elem(a : A) = item filter (_ == a)

  def readSeq[C <% Seq[A]](seq : C) : Rule[C] = if (seq isEmpty) success(seq)
      else seq.map(elem(_)).reduceLeft[Rule[A]](_ -~ _) -^ seq

  def choice[C <% Seq[A]](seq : C) : Rule[A] = select(seq.map(elem))

  /** Allows rules like 'a' to 'z' */
  implicit def iteratorToChoice[B <: Iterator[A]](iterator : B) : Rule[A] = choice(iterator.toList)

  def view[B](transform : Rule[B])(input : S) = new View[A, B, S](transform, input, 0)
}

     
/**
 * A Scanner is a parser for character input.
 */
trait Scanner extends Parser[Char] {
  implicit def readString(string : String) : Rule[String] = readSeq(string)
  implicit def stringToInput(string : String) : ArrayInput[Char] = new ArrayInput[Char](string.toArray)

  def toString(seq : Seq[Any]) = seq.mkString("")
    
  import Character._
  def whitespace = item filter isWhitespace *
  def newline = "\r\n" | "\n" | "\r"

  def trim[A](rule : Rule[A]) = whitespace -~ rule ~- whitespace
}
