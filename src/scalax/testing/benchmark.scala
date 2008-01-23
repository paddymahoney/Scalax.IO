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

package scalax.testing
import scala.collection.mutable._

class Benchmark(iters : Int) {
	def reps = 10
	def this() = this(1000000)

	class Is(v : String) {
		def is(f : => Unit) = tests += new Test(v, f _)
	}
	implicit def strToIs(v : String) = new Is(v)

	private class Test(name : String, f : () => Unit) {
		var runs = 0
		var total = 0L
		def heading() = {
			print(" ")
			for(val i <- name.length until 10) print(" ")
			print(name)
			print(" ")
		}
		def line() = {
			print("-")
			for(val i <- 0 until (if(name.length < 10) 10 else name.length)) print("-")
			print("-")
		}
		def reset() = {
			runs = 0
			total = 0L
		}
		def run() = {
			var i = 0
			val s = System.nanoTime
			while(i < iters) {
				f()
				i += 1
			}
			val e = System.nanoTime - s
			val str = (e / 1000000).toString
			print(" ")
			for(val i <- str.length until (if(name.length < 10) 10 else name.length)) print(" ")
			print(str)
			print(" ")
			runs += 1
			total += e
		}
		def avg() = {
			val str = (total / runs / 1000000).toString + " ms "
			print(" ")
			for(val i <- str.length until (if(name.length < 10) 10 else name.length)) print(" ")
			print(str)
			print(" ")
		}
		def ms() = {
			val str = ((1000L * iters * runs) / total).toString + " M/s"
			print(" ")
			for(val i <- str.length until (if(name.length < 10) 10 else name.length)) print(" ")
			print(str)
			print(" ")
		}
		def pct(std : Long) = {
			val str = ((std * 100) / total).toString + " %  "
			print(" ")
			for(val i <- str.length until (if(name.length < 10) 10 else name.length)) print(" ")
			print(str)
			print(" ")
		}
	}
	private val tests = new ListBuffer[Test]

	def main(argv : Array[String]) : Unit = {
		def row(f : Test => Unit) = {
			print("\n  ")
			tests.map(f)
		}
		def row2(f : Test => Unit) = {
			print("\n      ")
			tests.map(f)
		}
		def line() = {
			print("\n  ")
			tests.map(_.line())
			print("---")
		}
		row(_.heading())
		line()
		row(_.run())
		row(_.run())
		line()
		tests.foreach(_.reset())
		for(val i <- 0 until reps) {
			print("\n  ")
			tests.map(_.run())
		}
		line()
		row2(_.avg())
		row2(_.ms())
		row2(_.pct(tests(0).total))
		line()
		print("\n\n")
	}
}
