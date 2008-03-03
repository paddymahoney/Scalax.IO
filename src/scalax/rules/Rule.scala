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
    def apply[Out, A, X, Err](f : In => Result[Out, A, X, Err]) = createRule(f)
    def success[Out, A](out : Out, a : A) = apply { in => Success(out, a) }
    def failure[X](x : X) = apply { in => Failure(x) }
    def error[Err](err : Err) = apply { in => Error(err) }
    def unit[A](a : A) = apply { in => Success(in, a) }
    def read = apply { in => Success(in, in) }
  }
  
  def createRule[In, Out, A, X, Err](_name : String, f : In => Result[Out, A, X, Err]) : Rule[In, Out, A, X, Err] = new Rule[In, Out, A, X, Err] {
    val name = _name
    val factory = RuleFactory.this
    def apply(in : In) = f(in)
  }

  implicit def createRule[In, Out, A, X, Err](f : In => Result[Out, A, X, Err]) : Rule[In, Out, A, X, Err] = new Rule[In, Out, A, X, Err] {
    val name = RuleNamer.newName()
    val factory = RuleFactory.this
    def apply(in : In) = f(in)
  }

  implicit def seqRule[In, A, X, Err](rule : Rule[In, In, A, X, Err]) : SeqRule[In, A, X, Err] = new SeqRule(rule)
  
  def success[Out, A](out : Out, a : A) = from[Any] success(out, a)
  def failure[X](x : X) = from[Any] failure(x)
  def error[Err](err : Err) = from[Any] error(err)
}


trait Rule[-In, +Out, +A, +X, +Err] extends (In => Result[Out, A, X, Err]) { 
  val name : String
  val factory : RuleFactory
  
  import factory._
  
  def as(name : String) = createRule(name, this)
  
  def flatMap[Out2, B, X2 >: X, Err2 >: Err](fa2ruleb : A => Out => Result[Out2, B, X2, Err2]) = createRule { 
    in : In => apply(in) match {
      case Success(out, a) => fa2ruleb(a)(out)
      case f @ Failure(_) => f
      case err @ Error(_) => err
    }
  }
  
  def map[B](fa2b : A => B) = flatMap { a => out => Success(out, fa2b(a)) }

  def filter(f : A => Boolean) = flatMap { a => out => if(f(a)) Success(out, a) else Failure(()) }

  def orElse[In2 <: In, Out2 >: Out, A2 >: A, Y, Err2 >: Err](other : => Rule[In2, Out2, A2, Y, Err2]) = createRule { 
    in : In2 => apply(in) match {
      case s @ Success(_, _) => s
      case err @ Error(_) => err
      case _ => other(in)
    }
  }
  //def orError[X2 >: Err <: X] : Rule[In, Out, A, Nothing, X] = mapResult[Out, A, Nothing, X] { 
  //  case s @ Success(_, _) => s
  //  case Failure(x) => Error(x) 
  //  case err @ Error(_) => err
  //}
  
  //def noError : Rule[In, Out, A, Nothing] = mapResult
  
  def mapResult[Out2, B, Y, Err2](f : Result[Out, A, X, Err] => Result[Out2, B, Y, Err2]) = createRule { 
    in : In => f(apply(in))
  }
  
  //def partialMapResult[Out2 >: Out, A2 >: A, X2 >: X](f : PartialFunction[Result[Out, A, X], Result[Out2, A2, X2]]) = mapResult { 
  //  case result if f isDefinedAt result => f(result)
  //  case result => result
  //}
  
  def |[In2 <: In, Out2 >: Out, A2 >: A, Y, Err2 >: Err](other : => Rule[In2, Out2, A2, Y, Err2]) = orElse(other)

  def ^^[B](fa2b : A => B) = map(fa2b)
  
  def ^^?[B](pf : PartialFunction[A, B]) = filter (pf.isDefinedAt(_)) ^^ pf 
  
  def -^[B](b : B) = map { any => b }
 
  /** Maps an Error or Failure */
  def |^[Y](fx2y : X => Y) = createRule { in : In => apply(in) mapFailure fx2y }
    
  def |>[In2 <: In, Y](f : In2 => Y) = createRule { in : In2 => apply(in) mapFailure { any => f(in) } }
  
  def >>[Out2, B, X2 >: X, Err2 >: Err](fa2ruleb : A => Out => Result[Out2, B, X2, Err2]) = flatMap(fa2ruleb)
  
  def >->[Out2, B, X2 >: X, Err2 >: Err](fa2resultb : A => Result[Out2, B, X2, Err2]) = flatMap { a => any => fa2resultb(a) }
  
  def >>?[Out2, B, X2 >: X, Err2 >: Err](pf : PartialFunction[A, Rule[Out, Out2, B, X2, Err2]]) = filter(pf isDefinedAt _) flatMap pf
  
  def ~[Out2, B, X2 >: X, Err2 >: Err](next : => Rule[Out, Out2, B, X2, Err2]) = for (a <- this; b <- next) yield new ~(a, b)

  def ~-[Out2, B, X2 >: X, Err2 >: Err](next : => Rule[Out, Out2, B, X2, Err2]) = for (a <- this; b <- next) yield a
  
  def -~[Out2, B, X2 >: X, Err2 >: Err](next : => Rule[Out, Out2, B, X2, Err2]) = for (a <- this; b <- next) yield b
  
  def ~++[Out2, B >: A, X2 >: X, Err2 >: Err](next : => Rule[Out, Out2, Seq[B], X2, Err2]) = for (a <- this; b <- next) yield a :: b.toList

  def ~>[Out2, B, X2 >: X, Err2 >: Err](next : => Out => Result[Out2, A => B, X2, Err2]) = for (a <- this; fa2b <- next) yield fa2b(a)
  
  //def ~![Out2, B, X2 >: X, Err2 >: Err](next : => Rule[Out, Out2, B, X2, Err2]) = for (a <- this; b <- next orError) yield new ~(a, b)
  
  //def ~-![Out2, B, X2 >: X, Err2 >: Err](next : => Rule[Out, Out2, B, X2, Err2]) = for (a <- this; b <- next orError) yield a
  
  //def -~![Out2, B, X2 >: X, Err2 >: Err](next : => Rule[Out, Out2, B, X2, Err2]) = for (a <- this; b <- next orError) yield b
  
  /** Creates a rule that suceeds only if this rule would fail on the given context. */
  def unary_! = createRule[In, _ <: In, X, A, Err] { in : In => 
    apply(in) match {
      case Success(_, a) => Failure(a)
      case Failure(x) => Success(in, x)
      case err @ Error(_) => err
    }
  }
  
  /** Creates a rule that succeeds if this rule succeeds, but returns the original input. */
  def & = createRule[In, _ <: In, A, X, Err] { in : In => 
    apply(in) match {
      case Success(_, a) => Success(in, a)
      case f @ Failure(_) => f
      case err @ Error(_) => err
    }
  }
    
  def -[In2 <: In](exclude : => Rule[In2, Any, Any, Any, Nothing]) = !exclude -~ this
      
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
  def >~>[Out2, B1, B2, B >: A <% B1 ~ B2, C, X2 >: X, Err2 >: Err](f : (B1, B2) => Out => Result[Out2, C, X2, Err2]) = flatMap { a =>
    (a : B1 ~ B2) match { case b1 ~ b2 => f(b1, b2) } 
  }
  
  override def toString() = name
}
  

