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
  
  trait Functor[+A] { this : Fun[A] =>
    def map[B](f : A => B) : Fun[B]
  }

  trait Filter[+A] extends Functor[A] { this : Fun[A] =>
    def filter(f : A => Boolean) : Fun[A]
  }

  trait Plus[+A] { this : Fun[A] =>
    def plus[B >: A](other : => Fun[B]) : Fun[B]
  }
  
  trait OrElse[+A] { this : Fun[A] =>
    def orElse[B >: A](other : => Fun[B]) : Fun[B]
  }

  trait Zero extends Functor[Nothing] { this : Fun[Nothing] =>
    override def map[B](f : Nothing => B) : Fun[B] = this
  }

  trait ZeroFilter extends Filter[Nothing] { this : Fun[Nothing] =>
    def filter(f : Nothing => Boolean) : Fun[Nothing] = this
  }
  
  trait ZeroPlus extends Functor[Nothing] { this : Fun[Nothing] =>
    def plus[B](other : => Fun[B]) : Fun[B] = other
  }
  
  trait ZeroOrElse extends Functor[Nothing] { this : Fun[Nothing] =>
    def orElse[B](other : => Fun[B]) : Fun[B] = other
  }
}

/** One of the 'unit' definitions must be overriden in concrete subclasses */
trait UnitFunctors extends Functors {
  def unit : Fun[Unit] = unit(())
  def unit[A](a : => A) : Fun[A] = unit map { Unit => a }
}

trait FunctorsWithZero extends Functors {
  def zero : Fun[Nothing]
}



trait Monoidals extends UnitFunctors {
  type Fun[+A] <: Monoidal[A]
    
  implicit def app[A, B](fab : Fun[A => B]) = (fa : Fun[A]) => fa applyTo fab
  implicit def appUnit[A, B](a2b : A => B) = app(unit(a2b))
  
  /** One of 'and' and 'applyTo' definitions must be overriden in concrete subclasses */
  trait Monoidal[+A] extends Functor[A] { self : Fun[A] =>
    def and[B](fb : => Fun[B]) : Fun[(A, B)] = ((a : A) => (b : B) => (a, b))(this)(fb)
    def applyTo[B](fab : Fun[A => B]) : Fun[B] = fab and this map { case (f, a) => f(a) }
  }
}
