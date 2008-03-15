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
 * Rules that operate on sequential input
 */
trait Parsers[T] extends RuleFactory {
  type S
  type Parser[A] = Rule[S, S, A, Nothing]
  
  /** Succeeds with the first element of the input unless input is empty. */
  def item : Parser[T]

  implicit def elem(t : T) = item.filter(_ == t)

  def readSeq[C <% Seq[T]](seq : C) = 
    if (seq isEmpty) from[S].unit(seq)
    else seq.map(elem).reduceLeft[Parser[T]](_ -~ _) -^ seq

  def choice[C <% Seq[T]](seq : C) = seq.map(elem).reduceLeft[Parser[T]](_ | _)

  /** Allows rules like 'a' to 'z' */
  implicit def iteratorToChoice[TS <: Iterator[T]](iterator : TS) : Parser[T] = choice(iterator.toList)
  implicit def iteratorToChoiceSeq[TS <: Iterator[T]](iterator : TS) = seqRule(iteratorToChoice(iterator))
}

/**
 * Rules that operate on a sequence of characters.
 */
trait Scanners extends Parsers[Char] {
  implicit def readString(string : String) : Parser[String] = readSeq(string)

  def toString(seq : Seq[Any]) = seq.mkString("")
      
  import Character._
  def whitespace = item filter isWhitespace *
  def newline = "\r\n" | "\n" | "\r"

  def trim[A](rule : Parser[A]) = whitespace -~ rule ~- whitespace
}


trait StringScanners extends Scanners {
  type S = String
  
  val item = from[String] { 
    case "" => Failure
    case s => Success(s.substring(1), s.charAt(0)) 
  }
}
