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

import scalax.data._
import scalax.io._
import scalax.testing._

object CommandLineTests extends TestSuite("CommandLine") {
	object Test1 extends CommandLineParser {
		val opt1 = new StringOption('a', "abc", "ABC") with AllowAllButSelf
		val opt2 = new StringOption('b', "bcd", "BCD") with AllowAll
		val opt3 = new Flag("lonely", "Lonely") with AllowNone
	}

	object Test2 extends CommandLineParser {
		val opt1 = new IntOption('a', "abc", "ABC") with AllowAll
		val opt2 = new Flag('b', "bcd", "BCD") with AllowAll
		override def permitNonOptions = false
	}

	def assertNeg(v : Bistate[Any, Any], m : Any) = v match {
		case Positive(_) => fail(v.toString)
		case Negative(e) => assertEq(e, m)
	}

	def assertPos[A](v : Bistate[A, String])(body : A => Unit) = v match {
		case Positive(p) => body(p)
		case Negative(e) => fail(e)
	}

	"Exclusion 1" is assertPos(Test1.parse(Array("--lonely", "other"))) { res =>
		assert("present", res(Test1.opt3))
		assertEq("absent", res(Test1.opt1), None)
		assertEq("nonOpt", res.nonOptions, List("other"))
	}

	"Exclusion 2" is assertNeg(Test1.parse(Array("--lonely", "-afoo")),
		"Cannot combine --lonely with --abc.")
	"Exclusion 3" is assertNeg(Test1.parse(Array("-a", "foo", "--lonely")),
		"Cannot combine --lonely with --abc.")
	"Exclusion 4" is assertNeg(Test1.parse(Array("-afoo", "--abc==bar")),
		"Cannot combine --abc with --abc.")

	"Formats 1" is assertPos(Test1.parse(Array("--abc=foo", "--bcd", "=bar", "-bbaz"))) { res =>
		assertEq(res(Test1.opt1), Some("foo"))
		assertEq(res.all(Test1.opt2), List("=bar", "baz"))
		assertEq(res(Test1.opt2), Some("=bar"))
		assertEq(res.nonOptions, Nil)
	}

	"Formats 2" is assertPos(Test1.parse(Array("-a=foo", "-b", "bar"))) { res =>
		assertEq(res(Test1.opt1), Some("=foo"))
		assertEq(res(Test1.opt2), Some("bar"))
		assertEq(res.nonOptions, Nil)
		assertEq(res.all(Test1.opt3), 0)
	}

	"Count 1" is assertNeg(Test1.parse(Array("--lonely", "--lonely")),
		"Cannot combine --lonely with --lonely.")

	"Count 2" is assertPos(Test2.parse(Array("-bbb"))) { res =>
		assert(res(Test2.opt2))
		assertEq(res.all(Test2.opt2), 3)
	}

	"Int" is assertPos(Test2.parse(Array("-a5"))) { res =>
		assertEq(res(Test2.opt1), Some(5))
	}

	"Nonopt" is assertNeg(Test2.parse(Array("foo")),
		"This command does not accept any non-option arguments.")
}
