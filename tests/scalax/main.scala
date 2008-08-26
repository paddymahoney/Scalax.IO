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

import scalax.testing._

object ScalaxTests extends TestSuite("Scalax") {
	include(CommandLineTests)
	include(CsvTests)

	// scalax.control
	include(RefTests)
	include(ConcurrentLinkedListTests)

	// scalax.io
	include(StreamHelpTests)
	include(ResourcesTests)
	include(FileHelpTests)

	// scalax.logging
	include(LoggingTests)
}

class TestScalax {
	def testScalax() {
		val (p, n) = ScalaxTests.run();
		if (p != n) throw new AssertionError((n - p) + "tests failed.")
	}
}

// vim: set ts=4 sw=4 noet:
