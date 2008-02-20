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

object Rule {
  
  implicit def apply[In, Out, A, X](f : In => Result[Out, A, X]) : Rule[In, Out, A, X] = new Rule[In, Out, A, X] {
    def apply(in : In) = f(in)
  }
  
  def success[Out, A](out : Out, a : A) = Rule { in : Any => Success(out, a) }
  
  def failure[X](x : X) = Rule { in : Any => Failure(x) }
  
  def error[X](x : X) = Rule { in : Any => Error(x) }
}

object SeqRule {
  
  implicit def apply[In, Out <: In, A, X](f : In => Result[Out, A, X]) : SeqRule[In, A, X] = new SeqRule[In, A, X] {
    def apply(in : In) = f(in)
  }

  def read[In] = SeqRule { in : In => Success(in, in) }
  
  def unit[In, A](a : A) = SeqRule { in : In => Success(in, a) }
  
}

import Rule._

trait Rule[-In, +Out, +A, +X] extends (In => Result[Out, A, X]) { 
  
  def flatMap[Out2, B, X2 >: X](fa2ruleb : A => Out => Result[Out2, B, X2]) = Rule { 
    in : In => apply(in) match {
      case Success(out, a) => fa2ruleb(a)(out)
      case f @ Failure(_) => f
    }
  }
  
  def map[B](fa2b : A => B) = flatMap { a => out => Success(out, fa2b(a)) }

  def filter(f : A => Boolean) = flatMap { a => out => if(f(a)) Success(out, a) else Failure(()) }

  def orElse[In2 <: In, Out2 >: Out, A2 >: A, Y](other : => Rule[In2, Out2, A2, Y]) = Rule { 
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
  def unary_! = Rule[In, _ <: In, X, A] {
    in : In => apply(in) match {
      case Success(_, a) => Failure(a)
      case Failure(x) => Success(in, x)
    }
  }
  
  /** Creates a rule that succeeds if this rule succeeds, but returns the original input. */
  def & = Rule[In, _ <: In, A, X] {
    in => apply(in) match {
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

}

import SeqRule._

trait SeqRule[S, +A, +X] extends Rule[S, S, A, X] {
  
  def ? = SeqRule { 
    in : S => apply(in) match {
      case Success(out, a) => Success(out, Some(a))
      case _ => Success(in, None)
    }
  }

  /** Creates a rule that always succeeds with a Boolean value.  
   *  Value is 'true' if this rule succeeds, 'false' otherwise */
  def -? = ? map { _ isDefined }
        
  def * = SeqRule { 
    // tail-recursive function with reverse list accumulator
    def rep(in : S, acc : List[A]) : (S, List[A]) = apply(in) match {
       case Success(out, a) => rep(out, a :: acc)
       case _ => (in, acc)
    }
    in : S => rep(in, Nil) match { case (out, list) => Success(out, list.reverse) }
  }
  
  def + = this ~++ *
    
  def ~>*[B >: A](f : SeqRule[S, B => B, Any]) = for (a <- this; fs <- f*) yield fs.foldLeft[B](a) { (b, f) => f(b) }
    
  def ~*~[B >: A, X2 >: X](join : SeqRule[S, (B, B) => B, X2]) = {
    this ~>* (for (f <- join; a <- this) yield f(_ : B, a))
  }
  
  /** Repeats this rule one or more times with a separator (which is discarded) */
  def +/(sep : S => Result[S, Any, Any]) = this ~++ (sep -~ this *)

  /** Repeats this rule zero or more times with a separator (which is discarded) */
  def */(sep : S => Result[S, Any, Any]) = +/(sep) | unit[S, List[A]](Nil)
  
  def *~-[Out, Y](end : => Rule[S, Out, Any, Y]) = (this - end *) ~- end
  def +~-[Out, X2 >: X](end : => Rule[S, Out, Any, X2]) = (this - end +) ~- end

}

