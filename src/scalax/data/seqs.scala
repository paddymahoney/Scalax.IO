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

/** A Seq variant that requires only the elements method to be implemented.
 * Note that consequently the provided apply and length methods are O(n). */
trait IteratorSeq[+A] extends Seq[A] {
	def apply(i : Int) : A = elements.drop(i).next

	def length = {
		var i = 0
		val iter = elements
		while(iter.hasNext) {
			iter.next
			i += 1
		}
		i
	}

	// Default implementation would be O(n)
	override def isEmpty = !elements.hasNext
}
