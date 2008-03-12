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

abstract class AbstractResourcesTests(name : String) extends TestSuite(name) {
	implicit def toFileExtras(file : File) = new FileExtras(file)

	def testTmpDir = FileHelp.tmpDir / name
	
	override def setUp() {
		testTmpDir.mkdirs()
	}
	
	override def tearDown() {
		testTmpDir.deleteRecursively()
	}
}

object ReaderResourceTests extends AbstractResourcesTests("StringResourceTests") {
	"Slurp" is {
		val string = "abc123"
		val got = ReaderResource.string(string).slurp()
		assertEq(string, got)
	}
	
	"PumpTo" is {
		val f1 = testTmpDir / "f1"
		val f2 = testTmpDir / "f2"
		
		f1.writeLines(List("a", "b"))
		
		f1.reader.pumpTo(f2.writer)
		
		assertEq(List("a", "b"), f2.readLines())
	}
	
	"PumpTo opens Reader first" is {
		val f1 = testTmpDir / "f1"
		val f2 = testTmpDir / "f2"
		
		try {
			f1.reader.pumpTo(f2.writer)
			fail()
		} catch {
			case e: IOException => // expecting
		}
		
		deny(f2.exists)
	}
}

object InputStreamResourceTests extends AbstractResourcesTests("InputStreamResource") {

	"Slurp" is {
		val array = Array(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte)
		val got = InputStreamResource.bytes(array).slurp()
		assert(got deepEquals array)
	}
	
	"File URL" is {
		val f = testTmpDir / "File"
		f.writer.writeLines(List("a", "b"))
		// open frm file URL
		val g = InputStreamResource.url("file://" + f.getPath)
		assertEq(List("a", "b"), g.readLines())
	}
	
	// XXX: ClassPath URL test
}

object OutputStreamResourceTests extends AbstractResourcesTests("OutputStreamResourceTests") {
	"File Append" is {
		val f = testTmpDir / "f"
		f.appendOutputStream.writeLine("a")
		f.appendOutputStream.writeLine("b")
		
		assertEq(List("a", "b"), f.readLines())
	}
}

object ResourcesTests extends TestSuite("Resources") {
	include(ReaderResourceTests)
	include(InputStreamResourceTests)
	include(OutputStreamResourceTests)
}

// vim: set ts=4 sw=4 noet:
