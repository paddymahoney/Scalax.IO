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

package scalax.data

trait Tristate[+P, +N] {
	def posValue : Option[P]
	def negValue : Option[N]

	def posMap[A](f : P => A) : Tristate[A, N]
	def negMap[A](f : N => A) : Tristate[P, A]

	// We have to settle for f returning a Bistate here
	def posFlatmap[A >: P, B >: N](f : P => Bistate[A, B]) : Tristate[A, B]
	def negFlatmap[A >: P, B >: N](f : N => Bistate[A, B]) : Tristate[A, B]
}

trait Bistate[+P, +N] extends Tristate[P, N] {
	def posMap[A](f : P => A) : Bistate[A, N]
	def negMap[A](f : N => A) : Bistate[P, A]
	def posFlatmap[A >: P, B >: N](f : P => Bistate[A, B]) : Bistate[A, B]
	def negFlatmap[A >: P, B >: N](f : N => Bistate[A, B]) : Bistate[A, B]
}

case class Positive[+P](x : P) extends Bistate[P, Nothing] {
	def posValue = Some(x)
	def negValue = None
	def posMap[A](f : P => A) = Positive(f(x))
	def negMap[A](f : Nothing => A) = this
	def posFlatmap[A >: P, B](f : P => Bistate[A, B]) = f(x)
	def negFlatmap[A >: P, B](f : Nothing => Bistate[A, B]) = this
}

case class Negative[+N](x : N) extends Bistate[Nothing, N] {
	def posValue = None
	def negValue = Some(x)
	def posMap[A](f : Nothing => A) = this
	def negMap[A](f : N => A) = Negative(f(x))
	def posFlatmap[A, B >: N](f : Nothing => Bistate[A, B]) = this
	def negFlatmap[A, B >: N](f : N => Bistate[A, B]) = f(x)
}

case object Absent extends Tristate[Nothing, Nothing] {
	def posValue = None
	def negValue = None
	def posMap[A](f : Nothing => A) = this
	def negMap[A](f : Nothing => A) = this
	def posFlatmap[A, B](f : Nothing => Bistate[A, B]) = this
	def negFlatmap[A, B](f : Nothing => Bistate[A, B]) = this
}
