package scalax.rules


trait Monads {
  type M[+A]
  
  def unit[A](a : => A) : M[A]
  def zero : M[Nothing] = error("zero not defined")
  
  trait Functor[+A] { self : M[A] =>
    def map[B](f : A => B) : M[B]
  }
  
  trait Monad[+A] extends Functor[A] { self : M[A] =>
    def flatMap[B](f : A => M[B]) : M[B]
    def map[B](f : A => B) = flatMap { a => unit(f(a)) }
    def filter[B >: A](f : A => Boolean) = flatMap { a => if (f(a)) unit(a) else zero }
  }

  trait Plus[+A] { self : M[A] =>
    def plus[B >: A](other : => M[B]) : M[B]
  }
  
  trait OrElse[+A] { self : M[A] =>
    def orElse[B >: A](other : => M[B]) : M[B]
  }

  trait Zero extends Monad[Nothing] { self : M[Nothing] =>
    def flatMap[B](f : Nothing => M[B]) : M[B] = this
  }

  trait ZeroPlus extends Zero with Plus[Nothing] { self : M[Nothing] =>
    def plus[B](other : => M[B]) = other
  }

  trait ZeroOrElse extends Zero with OrElse[Nothing] { self : M[Nothing] =>
    def orElse[B](other : => M[B]) = other
  }
}

trait StateReader extends Monads {
  type S
  
  def get : M[S]
  def read[A](f : S => A) : M[A]
  def set(s : => S) : M[S]
  def update(f : S => S) : M[S]
}




  //This leads for example to a re-implementation of (part of) Option as:
/*
  object Option extends Monads {
    type M[+A] = Option[A]
    def unit[A](a : => A) = Some(a)
    override def zero = None
  }
  
  sealed abstract class Option[+A] extends Option.Monad[A] with Option.OrElse[A]

  case class Some[+A](value : A) extends Option[A] {
    def flatMap[B](f : A => Option[B]) = f(value)
    def orElse[B >: A](other : => Option[B]) = this
  }

  case object None extends Option[Nothing] with Option.ZeroOrElse
*/
