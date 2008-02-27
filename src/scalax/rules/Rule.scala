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

object RuleNamer {
  private var n = 0
  def newName() = { n += 1; "$" + (n - 1) }
}

trait RuleFactory {
  def from[In] = new {
    def apply[Out, A, X](f : In => Result[Out, A, X]) = createRule(f)
    def success[Out, A](out : Out, a : A) = apply { in => Success(out, a) }
    def failure[X](x : X) = apply { in => Failure(x) }
    def error[X](x : X) = apply { in => Error(x) }
    def unit[A](a : A) = apply { in => Success(in, a) }
    def read = apply { in => Success(in, in) }
  }
  
  def createRule[In, Out, A, X](_name : String, f : In => Result[Out, A, X]) : Rule[In, Out, A, X] = new Rule[In, Out, A, X] {
    val name = _name
    val factory = RuleFactory.this
    def apply(in : In) = f(in)
  }

  implicit def createRule[In, Out, A, X](f : In => Result[Out, A, X]) : Rule[In, Out, A, X] = new Rule[In, Out, A, X] {
    val name = RuleNamer.newName()
    val factory = RuleFactory.this
    def apply(in : In) = f(in)
  }

  implicit def seqRule[In, A, X](rule : Rule[In, In, A, X]) : SeqRule[In, A, X] = new SeqRule(rule)
  
  def success[Out, A](out : Out, a : A) = from[Any] success(out, a)
  def failure[X](x : X) = from[Any] failure(x)
  def error[X](x : X) = from[Any] error(x)
}


trait Rule[-In, +Out, +A, +X] extends (In => Result[Out, A, X]) { 
  val name : String
  val factory : RuleFactory
  
  import factory._
  
  def as(name : String) = createRule(name, this)
  
  def flatMap[Out2, B, X2 >: X](fa2ruleb : A => Out => Result[Out2, B, X2]) = createRule { 
    in : In => apply(in) match {
      case Success(out, a) => fa2ruleb(a)(out)
      case f @ Failure(_) => f
    }
  }
  
  def map[B](fa2b : A => B) = flatMap { a => out => Success(out, fa2b(a)) }

  def filter(f : A => Boolean) = flatMap { a => out => if(f(a)) Success(out, a) else Failure(()) }

  def orElse[In2 <: In, Out2 >: Out, A2 >: A, Y](other : => Rule[In2, Out2, A2, Y]) = createRule { 
    in : In2 => apply(in) match {
      case s @ Success(_, _) => s
      case Error(x : Y) => Error(x)
      case _ => other(in)
    }
  }
  
  def |[In2 <: In, Out2 >: Out, A2 >: A, Y](other : => Rule[In2, Out2, A2, Y]) = orElse(other)

  def ^^[B](fa2b : A => B) = map(fa2b)
  
  def ^^?[B](pf : PartialFunction[A, B]) = filter (pf.isDefinedAt(_)) ^^ pf 
  
  def -^[B](b : B) = map { any => b }
 
  def >>[Out2, B, X2 >: X](fa2ruleb : A => Out => Result[Out2, B, X2]) = flatMap(fa2ruleb)
  
  def >->[Out2, B, X2 >: X](fa2resultb : A => Result[Out2, B, X2]) = flatMap { a => any => fa2resultb(a) }
  
  def >>?[Out2, B, X2 >: X](pf : PartialFunction[A, Rule[Out, Out2, B, X2]]) = filter(pf isDefinedAt _) flatMap pf
  
  def ~[Out2, B, X2 >: X](next : => Rule[Out, Out2, B, X2]) = for (a <- this; b <- next) yield new ~(a, b)

  def ~-[Out2, B, X2 >: X](next : => Rule[Out, Out2, B, X2]) = for (a <- this; b <- next) yield a
  
  def -~[Out2, B, X2 >: X](next : => Rule[Out, Out2, B, X2]) = for (a <- this; b <- next) yield b
  
  def ~++[Out2, B >: A, X2 >: X](next : => Rule[Out, Out2, Seq[B], X2]) = for (a <- this; b <- next) yield a :: b.toList

  def ~>[Out2, B, X2 >: X](next : => Out => Result[Out2, A => B, X2]) = for (a <- this; fa2b <- next) yield fa2b(a)
  
  /** Creates a rule that suceeds only if this rule would fail on the given context. */
  def unary_! = createRule[In, _ <: In, X, A] { in : In => 
    apply(in) match {
      case Success(_, a) => Failure(a)
      case Failure(x) => Success(in, x)
    }
  }
  
  /** Creates a rule that succeeds if this rule succeeds, but returns the original input. */
  def & = createRule[In, _ <: In, A, X] { in : In => 
    apply(in) match {
      case Success(_, a) => Success(in, a)
      case f @ Failure(_) => f
    }
  }
    
  def -[In2 <: In](exclude : => Rule[In2, Any, Any, Any]) = !exclude -~ this
      
  /** ^~^(f) is equivalent to ^^ { case b1 ~ b2 => f(b1, b2) } 
   */
  def ^~^[B1, B2, B >: A <% B1 ~ B2, C](f : (B1, B2) => C) = map { a =>
    (a : B1 ~ B2) match { case b1 ~ b2 => f(b1, b2) } 
  }
    
  /** ^~~^(f) is equivalent to ^^ { case b1 ~ b2 ~ b3 => f(b1, b2, b3) } 
   */
  def ^~~^[B1, B2, B3, B >: A <% B1 ~ B2 ~ B3, C](f : (B1, B2, B3) => C) = map { a =>
    (a : B1 ~ B2 ~ B3) match { case b1 ~ b2 ~ b3 => f(b1, b2, b3) } 
  }
    
  /** ^~~~^(f) is equivalent to ^^ { case b1 ~ b2 ~ b3 ~ b4 => f(b1, b2, b3, b4) } 
   */
  def ^~~~^[B1, B2, B3, B4, B >: A <% B1 ~ B2 ~ B3 ~ B4, C](f : (B1, B2, B3, B4) => C) = map { a =>
    (a : B1 ~ B2 ~ B3 ~ B4) match { case b1 ~ b2 ~ b3 ~ b4 => f(b1, b2, b3, b4) } 
  }
     
  /** ^~~~~^(f) is equivalent to ^^ { case b1 ~ b2 ~ b3 ~ b4 ~ b5 => f(b1, b2, b3, b4, b5) } 
   */
  def ^~~~~^[B1, B2, B3, B4, B5, B >: A <% B1 ~ B2 ~ B3 ~ B4 ~ B5, C](f : (B1, B2, B3, B4, B5) => C) = map { a =>
    (a : B1 ~ B2 ~ B3 ~ B4 ~ B5) match { case b1 ~ b2 ~ b3 ~ b4 ~ b5 => f(b1, b2, b3, b4, b5) } 
  }
      
  /** ^~~~~~^(f) is equivalent to ^^ { case b1 ~ b2 ~ b3 ~ b4 ~ b5 ~ b6 => f(b1, b2, b3, b4, b5, b6) } 
   */
  def ^~~~~~^[B1, B2, B3, B4, B5, B6, B >: A <% B1 ~ B2 ~ B3 ~ B4 ~ B5 ~ B6, C](f : (B1, B2, B3, B4, B5, B6) => C) = map { a =>
    (a : B1 ~ B2 ~ B3 ~ B4 ~ B5 ~ B6) match { case b1 ~ b2 ~ b3 ~ b4 ~ b5 ~ b6 => f(b1, b2, b3, b4, b5, b6) } 
  }
       
  /** ^~~~~~~^(f) is equivalent to ^^ { case b1 ~ b2 ~ b3 ~ b4 ~ b5 ~ b6 => f(b1, b2, b3, b4, b5, b6) } 
   */
  def ^~~~~~~^[B1, B2, B3, B4, B5, B6, B7, B >: A <% B1 ~ B2 ~ B3 ~ B4 ~ B5 ~ B6 ~ B7, C](f : (B1, B2, B3, B4, B5, B6, B7) => C) = map { a =>
    (a : B1 ~ B2 ~ B3 ~ B4 ~ B5 ~ B6 ~ B7) match { case b1 ~ b2 ~ b3 ~ b4 ~ b5 ~ b6 ~b7 => f(b1, b2, b3, b4, b5, b6, b7) } 
  }
   
  /** >~>(f) is equivalent to >> { case b1 ~ b2 => f(b1, b2) } 
   */
  def >~>[Out2, B1, B2, B >: A <% B1 ~ B2, C, X2 >: X](f : (B1, B2) => Out => Result[Out2, C, X2]) = flatMap[Out2, C, X2] { a =>
    (a : B1 ~ B2) match { case b1 ~ b2 => f(b1, b2) } 
  }
  
  override def toString() = name
}

