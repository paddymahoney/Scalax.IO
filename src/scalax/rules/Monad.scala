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

trait Monads extends UnitFunctors {
  type Fun[+A] <: Monad[A]
  
  trait Monad[+A] extends Functor[A] { this : Fun[A] =>
    def flatMap[B](f : A => Fun[B]) : Fun[B]
    def map[B](f : A => B) = flatMap { a => unit(f(a)) }
  }
  
  trait Zero extends Monad[Nothing] with ZeroFilter { this : Fun[Nothing] =>
    def flatMap[B](f : Nothing => Fun[B]) : Fun[B] = this
  }
}


trait StateReader extends Monads {
  type S
  
  def get : Fun[S]
  def read[A](f : S => A) : Fun[A]
  def set(s : => S) : Fun[S]
  def update(f : S => S) : Fun[S]
}




