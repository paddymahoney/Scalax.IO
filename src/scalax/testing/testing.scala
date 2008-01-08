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

package scalax.testing
import scala.collection.mutable._

class TestFail(msg : String) extends Exception(msg)

trait TestCase {
	def name : String
	def run() : (Int, Int)
}

class TestSuite(val name : String) extends TestCase {
	class SingleTestCase(val name : String, code : => Unit) extends TestCase {
		def run() = {
			print("- "+name + "... ")
			try {
				code
				println("PASSED")
				(1, 1)
			} catch {
				case e : TestFail => {
					if(e.getMessage() == "") println("FAILED")
					else println("FAILED: "+e.getMessage())
					(0, 1)
				}
				case e : Throwable => {
					println("EXCEPTION")
					e.printStackTrace(System.out)
					(0, 1)
				}
			}
		}
	}

	private val tests = new ListBuffer[TestCase]
	class TestIs(n : String) {
		def is(code : => Unit) : Unit =
			tests += new SingleTestCase(n, code)
	}

	implicit def testIs(n : String) = new TestIs(n)
	def include(s : TestSuite) = tests += s

	def fail() = throw new TestFail("")
	def fail(msg : String) = throw new TestFail(msg)

	def assert(x : => Boolean) : Unit = assert("", x)
	def assert(msg : String, x : => Boolean) : Unit =
		if(!x) throw new TestFail("")
	def deny(x : => Boolean) = assert("", !x)
	def deny(msg : String, x : => Boolean) = assert(msg, !x)
	def assertEq(msg : String, x : Any, y : Any) =
		if(x != y) throw new TestFail(msg+": "+x+" != "+y)
	def assertEq(x : Any, y : Any) =
		if(x != y) throw new TestFail(x+" != "+y)
	def assertNe(msg : String, x : Any, y : Any) =
		if(x == y) throw new TestFail(msg+": "+x+" == "+y)
	def assertNe(x : Any, y : Any) =
		if(x == y) throw new TestFail(x+" == "+y)

	def run() = {
		println("Running "+name+"...")
		runThese(tests)
	}

	def runThese(ts : Seq[TestCase]) = {
		var n = 0
		var p = 0
		for(val t <- ts) {
			val r = t.run()
			p += r._1
			n += r._2
		}
		if(p == n) println(name+": all "+n+" tests passed.")
		else println(name+": "+(n - p)+" of "+n+" tests failed.")
		(p, n)
	}

	def main(argv : Array[String]) {
		val r =
			if(argv.length > 0) runThese(tests.filter(t => argv.contains(t.name)))
			else run()
		exit(if(r._1 == r._2) 0 else 1)
	}
}
