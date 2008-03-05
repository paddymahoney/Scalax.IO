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

import scalax.data._
import scalax.io._
import scalax.testing._
import java.io._

object StreamHelpTests extends TestSuite("StreamHelp") {
	"Lines 1" is {
		val r = StreamHelp.lines(new StringReader("a\nb\ncd"))
		assert(r.hasNext)
		assert(r.hasNext)
		assertEq("a", r.next())
		assertEq("b", r.next())
		assert(r.hasNext)
		assertEq("cd", r.next())
		
		// hasNext is called near EOF
		deny(r.hasNext)
		try {
			r.next()
			fail()
		} catch {
			case _ : NoSuchElementException =>
		}
		
		deny(r.hasNext)
		deny(r.hasNext)
		
		try {
			r.next()
			fail()
		} catch {
			case _ : NoSuchElementException =>
		}
	}
	
	"Lines 2" is {
		val r = StreamHelp.lines(new StringReader("a\nb\n"))
		r.next()
		r.next()
		
		// hasNext is not called at EOF
		try {
			r.next()
			fail()
		} catch {
			case _ : NoSuchElementException =>
		}
		
		try {
			r.next()
			fail()
		} catch {
			case _ : NoSuchElementException =>
		}
		
		deny(r.hasNext)
	}
}

// vim: set ts=4 sw=4 noet:
