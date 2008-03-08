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

object ReaderResourceTests extends TestSuite("StringResourceTests") {
	"Slurp" is {
		val string = "abc123"
		val got = ReaderResource.string(string).slurp()
		assertEq(string, got)
	}
}

object InputStreamResourceTests extends TestSuite("InputStreamResource") {
	"Slurp" is {
		val array = Array(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte)
		val got = InputStreamResource.bytes(array).slurp()
		assert(got deepEquals array)
	}
}

object ResourcesTests extends TestSuite("Resources") {
	include(ReaderResourceTests)
	include(InputStreamResourceTests)
}

// vim: set ts=4 sw=4 noet:
