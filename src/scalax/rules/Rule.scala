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

/** A Rule is a function from some input to a Result.  The result may be:
  * <ul>
  * <li>Success, with a value of some type and an output that may serve as the input to subsequent rules.</li> 
  * <li>Failure. A failure may result in some alternative rule being applied.</li>
  * <li>Error.  No further rules should be attempted.</li>
  * </ul>
  *
  * @author Andrew Foggin
  *   
  * Inspired by the Scala parser combinator.
  */
trait Rule[-In, +Out, +A, +X] extends (In => Result[Out, A, X]) { 
  val factory : Rules
  import factory._
  
  def as(name : String) = ruleWithName(name, this)
  
  def flatMap[Out2, B, X2 >: X](fa2ruleb : A => Out => Result[Out2, B, X2]) = mapResult { 
      case Success(out, a) => fa2ruleb(a)(out)
      case Failure => Failure
      case err @ Error(_) => err
  }
  
  def map[B](fa2b : A => B) = flatMap { a => out => Success(out, fa2b(a)) }

  def filter(f : A => Boolean) = flatMap { a => out => if(f(a)) Success(out, a) else Failure }

  def orElse[In2 <: In, Out2 >: Out, A2 >: A, X2 >: X](other : => Rule[In2, Out2, A2, X2]) = mapRule { 
    case s @ Success(_, _) => in : In2 => s
    case Failure => in : In2 => other(in)
    case err @ Error(_) => in : In2 => err
  }
  
  def orError = orElse(error[In])
  
  def mapResult[Out2, B, Y](f : Result[Out, A, X] => Result[Out2, B, Y]) = rule { 
    in : In => f(apply(in))
  }
  
  def mapRule[In2 <: In, Out2, B, Y](f : Result[Out, A, X] => In2 => Result[Out2, B, Y]) = rule { 
    in : In2 => f(apply(in))(in)
  }

  def |[In2 <: In, Out2 >: Out, A2 >: A, X2 >: X](other : => Rule[In2, Out2, A2, X2]) = orElse(other)

  def ^^[B](fa2b : A => B) = map(fa2b)
  
  def ^^?[B](pf : PartialFunction[A, B]) = filter (pf.isDefinedAt(_)) ^^ pf 
      
  def ??(pf : PartialFunction[A, Any]) = filter (pf.isDefinedAt(_))
  
  def -^[B](b : B) = map { any => b }
 
  /** Maps an Error */
  def !^[Y](fx2y : X => Y) = mapResult { 
    case s @ Success(_, _) => s
    case Failure => Failure
    case Error(x) => Error(fx2y(x))
  }
    
  def >>[Out2, B, X2 >: X](fa2ruleb : A => Out => Result[Out2, B, X2]) = flatMap(fa2ruleb)
  
  def >->[Out2, B, X2 >: X](fa2resultb : A => Result[Out2, B, X2]) = flatMap { a => any => fa2resultb(a) }
  
  def >>?[Out2, B, X2 >: X](pf : PartialFunction[A, Rule[Out, Out2, B, X2]]) = filter(pf isDefinedAt _) flatMap pf
  
  def ~[Out2, B, X2 >: X](next : => Rule[Out, Out2, B, X2]) = for (a <- this; b <- next) yield new ~(a, b)

  def ~-[Out2, B, X2 >: X](next : => Rule[Out, Out2, B, X2]) = for (a <- this; b <- next) yield a
  
  def -~[Out2, B, X2 >: X](next : => Rule[Out, Out2, B, X2]) = for (a <- this; b <- next) yield b
  
  def ~++[Out2, B >: A, X2 >: X](next : => Rule[Out, Out2, Seq[B], X2]) = for (a <- this; b <- next) yield a :: b.toList

  /** Apply the result of this rule to the function returned by the next rule */
  def ~>[Out2, B, X2 >: X](next : => Rule[Out, Out2, A => B, X2]) = for (a <- this; fa2b <- next) yield fa2b(a)
  
  /** Apply the result of this rule to the function returned by the previous rule */
  def <~:[InPrev, B, X2 >: X](prev : => Rule[InPrev, In, A => B, X2]) = for (fa2b <- prev; a <- this) yield fa2b(a)
    
  def ~![Out2, B, X2 >: X](next : => Rule[Out, Out2, B, X2]) = for (a <- this; b <- next orError) yield new ~(a, b)
  
  def ~-![Out2, B, X2 >: X](next : => Rule[Out, Out2, B, X2]) = for (a <- this; b <- next orError) yield a
  
  def -~![Out2, B, X2 >: X](next : => Rule[Out, Out2, B, X2]) = for (a <- this; b <- next orError) yield b
  
  /** Creates a rule that suceeds only if this rule would fail on the given context. */
  def unary_! = mapRule[In, _ <: In, Unit, Nothing] { 
    case Success(_, _) => in => Failure
    case _ => in => Success(in, ())
  }

  /** Creates a rule that succeeds if this rule succeeds, but returns the original input. */
  def & = mapRule[In, _ <: In, A, X] {
    case Success(_, a) => in => Success(in, a)
    case Failure => in => Failure
    case err : Error[_] => in => err
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
  def >~>[Out2, B1, B2, B >: A <% B1 ~ B2, C, X2 >: X](f : (B1, B2) => Out => Result[Out2, C, X2]) = flatMap { a =>
    (a : B1 ~ B2) match { case b1 ~ b2 => f(b1, b2) } 
  }

  /** ^-^(f) is equivalent to ^^ { b2 => b1 => f(b1, b2) } 
   */
  def ^-^ [B1, B2 >: A, C](f : (B1, B2) => C) = map { b2 : B2 => b1 : B1 => f(b1, b2) } 
    
 /** ^~>~^(f) is equivalent to ^^ { case b2 ~ b3 => b1 => f(b1, b2, b3) } 
  */
 def ^~>~^ [B1, B2, B3, B >: A <% B2 ~ B3, C](f : (B1, B2, B3) => C) = map { a => 
   (a : B2 ~ B3) match { case b2 ~ b3 => b1 : B1 => f(b1, b2, b3) } 
 }
     
  
}
  

