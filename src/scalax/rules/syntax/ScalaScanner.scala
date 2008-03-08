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



import ScalaScanner._
import Character._

/** A scanner for Scala source code.
 *
 * @author Andrew Foggin
 *
 * based on Scala Language Specification.
*/
trait ScalaScanner extends Scanner {
  /** Treat a symbol as a rule that matches the corresponding keyword */
  implicit def symbolToKeyword(symbol : Symbol) : Rule[String] = reservedId filter (_ == symbol.name)
  implicit def symbolToKeywordSeq(symbol : Symbol) = seqRule(symbolToKeyword(symbol))
     
  /** rule that sets multiple statements status and returns the previous value */
  def multiple(allow : Boolean) : Rule[Boolean]
  val multipleStatementsAllowed : Rule[Any]
    
  def lastTokenCanEndStatement(value : Boolean) : Rule[Any]
  val lastTokenCanEndStatement : Rule[Any]
    
  def singleStatement[T](rule : Rule[T]) = for (s <- multiple(false); t <- rule; _ <- multiple(s)) yield t
  def multipleStatements[T](rule : Rule[T]) = for (s <- multiple(true); t <- rule; _ <- multiple(s)) yield t
            
  val position : Rule[() => Int]
  def element[T](rule : Rule[T]) : Rule[Element[T]] = !nl -~ skip -~ position ~ rule ~ position ~- (space*) ^~~^ ScalaElement[T]
    
  def token[T](key : String, rule : Rule[T], f : T => Boolean) : Rule[T] = !nl -~ skip -~ rule ~- (space*) >> tokenCanEndStatement(f) as key
  def tokenCanEndStatement[T](f : T => Boolean)(t : T) = lastTokenCanEndStatement(f(t)) -~ success(t)
  def endToken[T](key : String, rule : Rule[T]) : Rule[T] = token(key, rule, { t : T => true })
    
  lazy val space = choice(" \t")
  lazy val skip = space | comment | newline *
  lazy val startStatement = skip ~- (
      choice("({") 
      | literal 
      | id 
      | reservedId.filter(canStartStatement) 
      | 'case ~ ('class | 'object))
    
  lazy val nl = (multipleStatementsAllowed 
      -~ lastTokenCanEndStatement 
      -~ (space | comment *) 
      -~ newline 
      ~- (startStatement &)) as "nl"

  def delim(char : Char) : Rule[Char] = !nl -~ skip -~ char ~- lastTokenCanEndStatement(")]}" contains char)
      
  lazy val semi = delim(';') | (nl+) as "semi"
  lazy val dot = delim('.')
  lazy val comma = delim(',')
    
  def round[T](rule : Rule[T]) = delim('(') -~ singleStatement(rule) ~- delim(')')
  def square[T](rule : Rule[T]) = delim('[') -~ singleStatement(rule) ~- delim(']')
  def curly[T](rule : Rule[T]) = delim('{') -~ multipleStatements(rule) ~- delim('}')
    
  def idToken(string : String) : Rule[String] = (plainId | reservedId) filter (_ == string)
    
  lazy val `_` = idToken("_")
  lazy val `:` = idToken(":")
  lazy val `=` = idToken("=")
  lazy val `=>` = idToken("=>") | idToken("\u21D2")
  lazy val `<-` = idToken("<-")
  lazy val `<:` = idToken("<:")
  lazy val `<%` = idToken("<%")
  lazy val `>:` = idToken(">:")
  lazy val `#` = idToken("#")
  lazy val `@` = idToken("@")
    
  lazy val `|` = idToken("|")
  lazy val `*` = idToken("*")
    
  lazy val plus = idToken("+")
  lazy val minus = idToken("-")
  lazy val bang = idToken("!")
  lazy val tilde = idToken("~")
    
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
    
  lazy val quoteId = endToken("quoteId", '`' -~ (printableChar +~- '`') ^^ toString)
  lazy val unquotedId = letter ~++ idRest | (opChar+)
  lazy val plainId = endToken("plainId", notReserved((unquotedId) ^^ toString))
  lazy val id = quoteId | plainId
  lazy val varId = plainId filter { id => id.charAt(0) isLowerCase }
  lazy val rightOp = plainId filter { id => id.endsWith(":") }
    
  val keyword = reserved(letter ~++ idRest ^^ toString) as "keyword"
  val reservedOp = reserved((opChar+) ^^ toString)
  lazy val reservedId = token("reservedId", keyword | reservedOp, canEndStatement)
    
  def reserved(rule : Rule[String]) = rule filter isReserved
  def notReserved(rule : Rule[String]) = rule filter isNotReserved
    
  val nonZero = decimalDigit filter (_ > 0)
  val hexNumeral = "0x" -~ hexDigit ~>* hex
  val octalNumeral = '0' -~ octalDigit ~>* oct
  val decimalNumeral = !'0' -~ decimalDigit ~>* dec | '0' -^ 0L
    
  val integerLiteral = (hexNumeral | octalNumeral | decimalNumeral) ~ (choice("Ll")-?) ~- !idChar >> {
    case value ~ false => success(IntegerLiteral(value.asInstanceOf[Int]))
    case value ~ true => success(LongLiteral(value))
  }
    
  val intPart = decimalNumeral ^^ (_ toString) | ""
  val floatPart = ('.' ~++ (('0' to '9')*) ^^ toString) | ""
  val exponentPart = choice("Ee") ~ ("+" | "-" | "") ~ intPart ^^ { case e ~ s ~ n => e + s + n } | ""

  val floatLiteral = intPart ~ floatPart ~ exponentPart ~ (letter ~++ idRest ^^ toString ?) >>? {
    case "" ~ ("" | ".") ~ _ ~ _ => failure // have to have at least one digit
    case _ ~ "" ~ "" ~ None => failure // it's an integer
    case i ~ f ~ e ~ Some("F" | "f") => success(FloatLiteral((i + f + e).toFloat))
    case i ~ f ~ e ~ (Some("D" | "d") | None)  => success(DoubleLiteral((i + f + e).toDouble))
  }

  val charElement = charEscapeSeq | printableChar
  val characterLiteral = '\'' -~ (charElement - '\'') ~- '\'' ^^ CharacterLiteral
  val stringLiteral = ("\"\"\"" -~ anyChar *~- "\"\"\"" | '\"' -~ charElement *~- '\"') ^^ toString ^^ StringLiteral
  val symbolLiteral = '\'' -~ unquotedId ^^ toString ^^ Symbol ^^ SymbolLiteral
    
  // note multi-line comments can nest
  lazy val multiLineComment : Rule[String] = ("/*" -~ (multiLineComment | anyChar) *~- "*/") ^^ toString
  val singleLineComment : Rule[String] = "//" -~ (item - newline *) ^^ toString
  lazy val comment = singleLineComment | multiLineComment as "comment"
    
  lazy val literal : Rule[Literal] = ('null -^ Null
      | 'true -^ True
      | 'false -^ False 
      | endToken("literalToken", (characterLiteral 
          | stringLiteral 
          | symbolLiteral
          | floatLiteral
          | integerLiteral) as "literal"))
}
