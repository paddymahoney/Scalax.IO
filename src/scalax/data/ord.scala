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

trait Ord[A] {
	def eq(a : A, b : A) : Boolean
	def ne(a : A, b : A) : Boolean = !eq(a, b)
	def le(a : A, b : A) : Boolean = lt(a, b) || eq(a, b)
	def lt(a : A, b : A) : Boolean
	def ge(a : A, b : A) : Boolean = !lt(a, b)
	def gt(a : A, b : A) : Boolean = !le(a, b)
}

class OrdIterableExtras[A](i : Iterable[A])(implicit ord : Ord[A]) {
	/** Returns the maximum element. Throws NoSuchElementException if there are none. */
	def max : A = {
		val e = i.elements
		var m = e.next
		for(v <- e) if(ord.gt(v, m)) m = v
		m
	}

	/** Returns the minimum element. Throws NoSuchElementException if there are none. */
	def min : A = {
		val e = i.elements
		var m = e.next
		for(v <- e) if(ord.lt(v, m)) m = v
		m
	}
}
