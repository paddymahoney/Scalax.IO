package scalax.rules

trait Arrows {
  type Arr[-A, +B] <: Arrow[A, B]

  def arrow[A, B](f : A => B) : Arr[A, B]
  def diag[A] = arrow[A, (A, A)] { a => (a, a) }

  trait Arrow[-A, +B] { self : Arr[A, B] =>
    def map[C](f : B => C) = comp(arrow(f))
    def comp[C](bc : => Arr[B, C]) : Arr[A, C]
    def fst[C] : Arr[(A, C), (B, C)]
  }
}

trait MonadArrows extends Monads with Arrows {
  type Arr[-A, +B] = MonadArrow[A, B]

  def arrow[A, B](f : A => B) = new Arr[A, B](a => unit(f(a)))

  class MonadArrow[-A, +B](val f : A => Fun[B]) extends Arrow[A, B] {
    def comp[C](bc : => Arr[B, C]) = new Arr[A, C](a => for (b <- f(a); c <- bc.f(b)) yield c) //(a => f(a) flatMap bc.f)
    def fst[C] = new Arr[(A, C), (B, C)]({ case (a,c) => for(b <- f(a)) yield (b,c) }) //({ case (a, c) => f(a) map { b => (b, c) } })
  }
}

trait ApplicativeArrows extends Arrows {
  type Arr[-A, +B] <: ApplicativeArrow[A, B]

  def app[A, B] : Arr[(Arr[A, B], A), B]
  
  trait ApplicativeArrow[-A, +B] extends Arrow[A, B] { self : Arr[A, B] =>
    def flatMap[SubA <: A, C](f : B => Arr[SubA, C]) : Arr[SubA, C] =
      diag[SubA].comp(map(f).fst[SubA]).comp(app[SubA, C])
  }
}

trait ArrowMonads extends ApplicativeArrows with Monads {
  type Arr[-A, +B] <: ApplicativeArrow[A, B] with Monad[B]
  type Fun[+A] = Arr[Nothing, A]

  override def unit[A](a : => A) : Fun[A] = arrow[Unit, A](Unit => a)
}
