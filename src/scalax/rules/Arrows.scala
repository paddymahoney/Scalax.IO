package scalax.rules

trait Arrows {
  type Arr[-X, +Y] <: Arrow[X, Y]

  def arrow[X, Y](f : X => Y) : Arr[X, Y]
  def tuple[X] = arrow[X, (X, X)] { x => (x, x) }

  trait Arrow[-X, +Y] { self : Arr[X, Y] =>
    def map[Z](f : Y => Z) = comp(arrow(f))
    def comp[Z](yz : => Arr[Y, Z]) : Arr[X, Z]
    def fst[Z] : Arr[(X, Z), (Y, Z)]
  }
}

trait MonadicArrows extends Monads with Arrows {
  type Arr[-X, +Y] = MonadicArrow[X, Y]

  def arrow[X, Y](f : X => Y) = new Arr[X, Y](x => unit(f(x)))

  class MonadicArrow[-X, +Y](val f : X => M[Y]) extends Arrow[X, Y] { self : Arr[X, Y] =>
    def comp[Z](yz : => Arr[Y, Z]) = new Arr[X, Z](x => f(x) flatMap yz.f)
    def fst[Z] = new Arr[(X, Z), (Y, Z)]({ case (x, z) => f(x) map { y => (y, z) } })
  }
}

trait ApplicativeArrows extends Arrows {
  type Arr[-X, +Y] <: ApplicativeArrow[X, Y]

  def app[X, Y] : Arr[(Arr[X, Y], X), Y]
  
  trait ApplicativeArrow[-X, +Y] extends Arrow[X, Y] { self : Arr[X, Y] =>
    def flatMap[SubX <: X, Z](f : Y => Arr[SubX, Z]) : Arr[SubX, Z] =
      tuple[SubX].comp(map(f).fst[SubX]).comp(app[SubX, Z])
  }
}

trait ArrowMonads extends ApplicativeArrows with Monads {
  type Arr[-X, +Y] <: ApplicativeArrow[X, Y] with Monad[Y]
  type M[+X] = Arr[Nothing, X]

  def unit[X](x : => X) : M[X] = arrow[Unit, X](Unit => x)
}
