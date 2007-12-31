// -----------------------------------------------------------------------------
//
//  Scalax - The Scala Community Library
//  Copyright (c) 2005-7 The Scalax Project. All rights reserved.
//
//  The primary distribution site is http://scalax.scalaforge.org/
//
//  This software is released under the terms of the Revised BSD License.
//  There is NO WARRANTY.  See the file LICENSE for the full text.
//
// -----------------------------------------------------------------------------

package scalax.control

/** Represents functions which have (potentially partial) inverses, and that
 * can therefore be used both in expressions and pattern matches. */
trait Injection[A, B] extends Function1[A, B] { self =>
	def unapply(x : B) : Option[A]
	def andThen[C](that : Injection[B, C]) : Injection[A, C] = that compose this
	def compose[C](that : Injection[C, A]) : Injection[C, B] =
		new Injection[C, B] {
			def apply(x : C) = self(that(x))
			def unapply(x : B) = self.unapply(x).flatMap(that.unapply)
		}
}
