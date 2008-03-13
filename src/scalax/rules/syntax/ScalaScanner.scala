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

/** A scanner for Scala source code.
 *
 * @author Andrew Foggin
 *
 * based on Scala Language Specification.
*/
object ScalaScanner {
  val reserved = Set(
      "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
      "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object",
      "override", "package", "private", "protected", "requires", "return", "sealed", "super", "this", 
      "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield",
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2")

  /** Reserved ids that can terminate a statement */
  val endStatements = Set("this", "null", "true", "false", "return", "type", "_")
    
  /** Reserved ids that cannot start a statement 
   *
   * Note: "case" cannot start statement unless followed by "class" or "object" 
   */
  val cannotStartStatements = Set(
      "case", "catch", "else", "extends", "finally", "forSome", "match", "requires", "with", "yield",
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "\u21D2")

  def isReserved(id : String) = reserved.contains(id)
  def isNotReserved(id : String) = !reserved.contains(id)
  def canStartStatement(id : String) = !cannotStartStatements.contains(id)
  def canEndStatement(id : String) = endStatements.contains(id)
}

class Position {
  
}

abstract class Token
abstract class NameToken extends Token {
  def name : String
}
case class ReservedId(name : String) extends NameToken
case class QuoteId(name : String) extends NameToken
case class PlainId(name : String) extends NameToken
case class Delimiter(delim : Char) extends Token
case class LiteralToken[+T](value : T) extends Token with Literal
case class Comment(text : String) extends Token
case object NewLineToken extends Token

import ScalaScanner._
import Character._

/** A scanner for Scala source code.
 *
 * @author Andrew Foggin
 *
 * based on Scala Language Specification.
*/
trait ScalaScanner extends Scanner with MemoisableRules {
  
  /** rule that sets multiple statements status and returns the previous value */
  def multiple(allow : Boolean) : Rule[Boolean]
  def multipleStatementsAllowed : Rule[Any]
  def lastTokenCanEndStatement(value : Boolean) : Rule[Any]
  def lastTokenCanEndStatement : Rule[Any]
  
  implicit def symbolToId(symbol : Symbol) : Rule[Token] = otherToken ?? { case ReservedId(symbol.name) => }

  lazy val token : Rule[Token] = nl | otherToken as "token"
  lazy val otherToken : Rule[Token] = skip -~ (literal | delimiter | id) as "otherToken"
  lazy val skip = space | comment | newline *
  
  lazy val nl : Rule[Token] = for {
    _ <- multipleStatementsAllowed;
    _ <- lastTokenCanEndStatement;
    _ <- space | comment *;
    _ <- newline;
    _ <- startStatement &
  } yield NewLineToken

  lazy val startStatement = 'case ~ ('class | 'object) | otherToken ?? {
    case Delimiter('(') | Delimiter('{') =>
    case _ : Literal =>
    case QuoteId(_) =>
    case PlainId(_) =>
    case ReservedId(name) if canStartStatement(name) =>
  }
    
  lazy val delimiter : Rule[Delimiter] = 
      choice("({[;,.") ~- lastTokenCanEndStatement(false) ^^ Delimiter |
      choice(")}]") ~- lastTokenCanEndStatement(true) ^^ Delimiter

  lazy val id =
      quoteId ~- lastTokenCanEndStatement(true) ^^ QuoteId |
      reservedId >> { id => success(id) ~- lastTokenCanEndStatement(endStatements(id.name)) } |
      plainId ~- lastTokenCanEndStatement(true) ^^ PlainId
    
  lazy val quoteId = '`' -~ (printableChar +~- '`') ^^ toString
  lazy val plainId = (letter ~++ idRest | (opChar+)) ^^ toString as "plainId"
  lazy val reservedId = (plainId filter isReserved) ^^ ReservedId
        
  // note multi-line comments can nest
  lazy val multiLineComment : Rule[String] = ("/*" -~ (multiLineComment | anyChar) *~- "*/") ^^ toString
  lazy val singleLineComment : Rule[String] = "//" -~ (item - newline *) ^^ toString
  lazy val comment = (singleLineComment | multiLineComment) ^^ Comment as "comment"
    
  
  lazy val literal : Rule[LiteralToken[Any]] = 
      (plainId ^^? {
        case "null" => LiteralToken(null)
        case "true" => LiteralToken(true)
        case "false" => LiteralToken(false)
      } |
      characterLiteral |
      stringLiteral |
      symbolLiteral |
      floatLiteral |
      integerLiteral) ~- lastTokenCanEndStatement(true) as "literal"
  
  lazy val charElement = charEscapeSeq | printableChar
  lazy val characterLiteral = '\'' -~ (charElement - '\'') ~- '\'' ^^ LiteralToken[Char]
  lazy val stringLiteral = ("\"\"\"" -~ anyChar *~- "\"\"\"" | '\"' -~ charElement *~- '\"') ^^ toString ^^ LiteralToken[String]
  lazy val symbolLiteral = '\'' -~ plainId ^^ Symbol ^^ LiteralToken[Symbol]
    
  lazy val space = choice(" \t")
    
  val decimalDigit = ('0' to '9') ^^ (_ - 48L)
  val octalDigit = decimalDigit.filter(_ < 8)
  val hexDigit = decimalDigit | ('A' to 'F') ^^ (_ - 55L) | ('a' to 'f') ^^ (_ - 87L)
    
  val dec = decimalDigit ^^ { d => n : Long => 10 * n + d }
  val oct = octalDigit ^^ { d => n : Long => 8 * n + d }
  val hex = hexDigit ^^ { d => n : Long => 16 * n + d }
    
  val unicodeEscape = "\\u" -~ hexDigit ~> hex ~> hex ~> hex ^^ { _.asInstanceOf[Char] }
  val octalEscape = '\\' -~ octalDigit ~> oct ~> oct ^^ { _.asInstanceOf[Char] }
   
  val charEscapeSeq = '\\' -~ ( choice("\"\'\\")
      | 'b' -^ '\b' | 't' -^ '\t' | 'n' -^ '\n' | 'f' -^ '\f' | 'r' -^ '\r') 

  val anyChar = unicodeEscape | octalEscape | item
  val printableChar = !choice("\b\t\n\f\r") -~ anyChar
    
  def unicode(category : Int) = anyChar filter (getType(_) == category)
    
  val letter = choice("$_") | (anyChar filter isLetter)
  val digit = anyChar filter isDigit
  val lower = anyChar filter isLowerCase
  val idChar = letter | digit
  val opChar = unicode(MATH_SYMBOL) | unicode(OTHER_SYMBOL) | choice("!#%&*+-/:<=>?@\\^|~")
  lazy val idRest : Rule[List[Char]] = ('_' ~++ (opChar+)) ~- !idChar | !idChar -^ Nil | idChar ~++ idRest
    
  val nonZero = decimalDigit filter (_ > 0)
  val hexNumeral = "0x" -~ hexDigit ~>* hex
  val octalNumeral = '0' -~ octalDigit ~>* oct
  val decimalNumeral = !'0' -~ decimalDigit ~>* dec | '0' -^ 0L
    
  val integerLiteral = (hexNumeral | octalNumeral | decimalNumeral) ~ (choice("Ll")-?) ~- !idChar >> {
    case value ~ false => success(LiteralToken(value.asInstanceOf[Int]))
    case value ~ true => success(LiteralToken(value))
  }
    
  val intPart = decimalNumeral ^^ (_ toString) | ""
  val floatPart = ('.' ~++ (('0' to '9')*) ^^ toString) | ""
  val exponentPart = choice("Ee") ~ ("+" | "-" | "") ~ intPart ^^ { case e ~ s ~ n => e + s + n } | ""

  val floatLiteral = intPart ~ floatPart ~ exponentPart ~ (letter ~++ idRest ^^ toString ?) >>? {
    case "" ~ ("" | ".") ~ _ ~ _ => failure // have to have at least one digit
    case _ ~ "" ~ "" ~ None => failure // it's an integer
    case i ~ f ~ e ~ Some("F" | "f") => success(LiteralToken((i + f + e).toFloat))
    case i ~ f ~ e ~ (Some("D" | "d") | None)  => success(LiteralToken((i + f + e).toDouble))
  }
}
