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

package scalax.data

// This looks a bit strange, as we're basically defining the same trait twice.
// The rationale is that client code should be easily readable using natural
// method naming, even for people who don't know/care what monoids are.

/** Type-class for monoids, where the operation looks like addition. */
trait MonoidWithPlus[A] {
	def plus(a : A, b : A) : A
	def zero : A
}

/** Type-class for monoids, where the operation looks like concatenation. */
trait MonoidWithJoin[A] {
	def join(a : A, b : A) : A
	def empty : A

	// Defined here to permit overriding with more efficient implementations.
	def concat(i : Iterable[A]) = {
		var t = empty
		for(v <- i) t = join(t, v)
		t
	}
}

class MonoidWithPlusIterableExtras[A](i : Iterable[A])(monoid : MonoidWithPlus[A]) {
	def sum = {
		var t = monoid.zero
		for(v <- i) t = monoid.plus(t, v)
		t
	}
}

class MonoidWithJoinIterableExtras[A](i : Iterable[A])(monoid : MonoidWithJoin[A]) {
	def concat = monoid.concat(i)
}
