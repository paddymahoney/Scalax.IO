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
  
  override def unit[A](a : => A) : Fun[A]
  
  trait Monad[+A] extends Functor[A] { self : Fun[A] =>
    def flatMap[B](f : A => Fun[B]) : Fun[B]
    def map[B](f : A => B) = flatMap { a => unit(f(a)) }
  }

  trait Plus[+A] { self : Fun[A] =>
    def plus[B >: A](other : => Fun[B]) : Fun[B]
  }
}


trait MonadsWithZero extends Monads {
  def zero : Fun[Nothing]
  
  trait MonadWithZero[+A] extends Monad[A] { self : Fun[A] =>
    def filter[B >: A](f : A => Boolean) = flatMap { a => if (f(a)) unit(a) else zero }
  }

  trait OrElse[+A] { self : Fun[A] =>
    def orElse[B >: A](other : => Fun[B]) : Fun[B]
  }

  trait Zero extends Monad[Nothing] { self : Fun[Nothing] =>
    def flatMap[B](f : Nothing => Fun[B]) : Fun[B] = this
  }

  trait ZeroPlus extends Zero with Plus[Nothing] { self : Fun[Nothing] =>
    def plus[B](other : => Fun[B]) = other
  }

  trait ZeroOrElse extends Zero with OrElse[Nothing] { self : Fun[Nothing] =>
    def orElse[B](other : => Fun[B]) = other
  }
}

trait StateReader extends Monads {
  type S
  
  def get : Fun[S]
  def read[A](f : S => A) : Fun[A]
  def set(s : => S) : Fun[S]
  def update(f : S => S) : Fun[S]
}




  //This leads for example to a re-implementation of (part of) Option as:
/*
  object Option extends MonadsWithZero {
    type Fun[+A] = Option[A]
    def unit[A](a : => A) = Some(a)
    override def zero = None
  }
  
  sealed abstract class Option[+A] extends Option.MonadWithZero[A] with Option.OrElse[A]

  case class Some[+A](value : A) extends Option[A] {
    def flatMap[B](f : A => Option[B]) = f(value)
    def orElse[B >: A](other : => Option[B]) = this
  }

  case object None extends Option[Nothing] with Option.ZeroOrElse
*/
