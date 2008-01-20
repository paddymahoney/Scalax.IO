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

import java.io._
import scalax.data._
import scalax.io._
import scalax.testing._

object CsvTests extends TestSuite("CSV") {
	def eqLists[A](a : List[A], b : List[A]) =
		(a zip b zipWithIndex).map {
			case ((x, y), i) => assertEq(i.toString, x, y)
		}

	"CSV" is {
		val csv =
			"""firstname, lastname
			|Foo, Bar,   baz@x.com   ,    "bel""as, dsfg",RG6 47, 3567 #Foo
			|
			|   ,,"1
			|2  "   ,
			|,,nix
			|""
			|
			|""".stripMargin
		val exp = List[List[String]](
			List("firstname", "lastname"),
			List("Foo", "Bar", "baz@x.com", "bel\"as, dsfg", "RG6 47", "3567 #Foo"),
			List(""),
			List("", "", "1\n2  ", ""),
			List("", "", "nix"),
			List(""),
			List("")
		)
		val out = new CsvIterator(new StringReader(csv)).map(_.toList).toList
		eqLists(exp, out)
	}

	"Key value" is {
		val kv =
			"""x = y
			|
			|# Test
			|blank=  # Blank
			|  nb = "#NB" #Comment
			|      # Another
			|  
			|""".stripMargin
		val out = new KeyValueIterator(new StringReader(kv)).toList
		val exp = List(
			("x", "y"),
			("blank", ""),
			("nb", "#NB")
		)
		eqLists(exp, out)
	}
}
