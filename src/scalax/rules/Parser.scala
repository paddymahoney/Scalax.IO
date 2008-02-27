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
trait SeqRules[A] extends Rules {
  /** Succeeds with the first element of the input unless input is empty. */
  def item : Rule[A]

  implicit def elem(a : A) : Rule[A] = item filter (_ == a)

  def readSeq[C <% Seq[A]](seq : C) : Rule[C] = if (seq isEmpty) success(seq)
      else seq.map(elem(_)).reduceLeft[Rule[A]](_ -~ _) -^ seq

  def choice[C <% Seq[A]](seq : C) : Rule[A] = select(seq.map(elem))

  /** Allows rules like 'a' to 'z' */
  implicit def iteratorToChoice[B <: Iterator[A]](iterator : B) : Rule[A] = choice(iterator.toList)
  implicit def iteratorToChoiceSeq[B <: Iterator[A]](iterator : B) : SeqRule[S, A, Any] = seqRule(iteratorToChoice(iterator))
}

       
/**
 * Rules that operate on a sequence of characters.
 */
trait CharSeqRules extends SeqRules[Char] {
  implicit def readString(string : String) : Rule[String] = readSeq(string) as string
  implicit def stringToInput(string : String) : ArrayInput[Char] = new ArrayInput[Char](string.toArray)

  def toString(seq : Seq[Any]) = seq.mkString("")
      
  import Character._
  def whitespace = item filter isWhitespace *
  def newline = "\r\n" | "\n" | "\r"

  def trim[A](rule : Rule[A]) = whitespace -~ rule ~- whitespace
}


trait StringScanner extends CharSeqRules {
  type S = String
  
  val item = rule { 
    case "" => Failure("End of input")
    case s => Success(s.substring(1), s.charAt(0)) 
  }
}




/**
 * A Parser is used to define rules that operate on sequential input
 */
trait Parser[A] extends SeqRules[A] {
  type S <: Input[A, S]

  /** Succeeds with the first element of the input unless input is empty. */
  val item = rule[A] { input => input.next }

  def view[B](transform : Rule[B])(input : S) = new View[A, B, S](transform, input, 0)
}

     
/**
 * A Scanner is a parser for character input.
 */
trait Scanner extends Parser[Char] with CharSeqRules