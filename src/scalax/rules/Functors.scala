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
  type F[+A] <: Functor[A]
  
  trait Functor[+A] { self : F[A] =>
    def map[B](f : A => B) : F[B]
  }
}


trait MonoidalFunctors extends Functors {
  type F[+A] <: MonoidalFunctor[A]
  
  def unit : F[Unit]
  def functor[A](a : => A) : F[A]
  
  trait MonoidalFunctor[+A] extends Functor[A] { self : F[A] =>
    def tuple[B](b : => F[B]) : F[(A, B)]
    def apply[B](fab : => F[A => B]) : F[B]
    //def apply2[B, C](fabc : => F[A => B => C])(fb : => F[B]) = fb(apply(fabc))
  }
}


trait ApplicativeFunctors extends MonoidalFunctors {
  def unit : F[Unit] = functor(())

  trait ApplicativeFunctor[+A] extends MonoidalFunctor[A] { self : F[A] =>
    def tuple[B](fb : => F[B]) = fb(apply(functor((a:A) => (b:B) => (a,b))))
    //def tuple[B](fb : => F[B]) = apply2(functor((a:A) => (b:B) => (a,b)))(fb)
  }
}


trait Monoids extends MonoidalFunctors {
  def functor[A](a : => A) : F[A] = unit.map[A](Unit => a)

  trait Monoid[+A]  extends MonoidalFunctor[A] { self : F[A] =>
    def apply[B](fab : => F[A => B]) : F[B] = tuple(fab) map { case (a, f) => f(a) }
  }
}
