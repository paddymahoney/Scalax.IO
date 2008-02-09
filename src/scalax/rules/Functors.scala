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

trait Functors {
  type Fun[+A] <: Functor[A]
  
  trait Functor[+A] { self : Fun[A] =>
    def map[B](f : A => B) : Fun[B]
  }
}

/** One of the 'unit' definitions must be overriden in concrete subclasses */
trait UnitFunctors extends Functors {
  def unit : Fun[Unit] = unit(())
  def unit[A](a : => A) : Fun[A] = unit map { Unit => a }
}



trait MonoidalFunctors extends UnitFunctors {
  type Fun[+A] <: MonoidalFunctor[A]
    
  implicit def app[A, B](fab : Fun[A => B]) = (fa : Fun[A]) => fa applyTo fab
  implicit def appUnit[A, B](a2b : A => B) = app(unit(a2b))
  
  /** One of 'and' and 'applyTo' definitions must be overriden in concrete subclasses */
  trait MonoidalFunctor[+A] extends Functor[A] { self : Fun[A] =>
    def and[B](fb : => Fun[B]) : Fun[(A, B)] = ((a : A) => (b : B) => (a, b))(this)(fb)
    def applyTo[B](fab : Fun[A => B]) : Fun[B] = fab and this map { case (f, a) => f(a) }
  }
}


//trait MonadicMonoidalFunctors extends Monads with MonoidalFunctors {
//  type Fun[+A] <: MonadicMonoidalFunctor[A]
//
//  class MonadicMonoidalFunctor[+A](val fa : Fun[A]) extends Monad[A] with MonoidalFunctor[A] { self : Fun[A] =>
//   def flatMap[B](a2fb : A => Fun[B]) : Fun[B] = fa flatMap a2fb
//   override def and[B](fb : => Fun[B]) : Fun[(A,B)] = for (a <- fa;  b <- fb) yield (a,b)
//  }
//}

