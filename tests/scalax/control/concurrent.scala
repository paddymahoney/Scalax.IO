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

import scalax.control._
import scalax.testing._

object ConcurrentLinkedListTests extends TestSuite("ConcurrentLinkedList") {
	// Not going to attempt to verify concurrency properties, but it should at
	// least work as expected in a single thread.

	"Length" is {
		val lst = new ConcurrentLinkedList[Int]
		assertEq(lst.length, 0)
		lst += 1
		assertEq(lst.length, 1)
		lst += 2
		assertEq(lst.length, 2)
		assertEq(lst(0), 1)
		assertEq(lst(1), 2)
		lst.clear()
		assertEq(lst.length, 0)
	}

	"Remove" is {
		val lst = new ConcurrentLinkedList[Int]
		lst += 1
		lst += 2
		lst += 3

		lst -= 5
		assertEq(lst.toList, List(1, 2, 3))
		lst -= 2
		assertEq(lst.toList, List(1, 3))
		lst -= 3
		assertEq(lst.toList, List(1))
		lst -= 1
		assertEq(lst.toList, Nil)
	}
}
