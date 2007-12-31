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

package scalax.data
import scala.collection.mutable._

object Implicits {
	implicit def stringExtras(s : String) = new StringExtras(s)
	implicit def intExtras(i : Int) = new IntExtras(i)

	implicit object IntProps extends Ord[Int] with MonoidWithPlus[Int] {
		def eq(a : Int, b : Int) = a == b
		def lt(a : Int, b : Int) = a < b
		def plus(a : Int, b : Int) = a + b
		def zero = 0
	}

	implicit object LongProps extends Ord[Long] with MonoidWithPlus[Long] {
		def eq(a : Long, b : Long) = a == b
		def lt(a : Long, b : Long) = a < b
		def plus(a : Long, b : Long) = a + b
		def zero = 0L
	}

	implicit object DoubleProps extends Ord[Double] with MonoidWithPlus[Double] {
		def eq(a : Double, b : Double) = a == b
		def lt(a : Double, b : Double) = a < b
		def plus(a : Double, b : Double) = a + b
		def zero = 0.0
	}

	implicit def listProps[A] = new MonoidWithJoin[List[A]] {
		def join(a : List[A], b : List[A]) = a ::: b
		def empty = Nil
		override def concat(i : Iterable[List[A]]) = {
			val b = new ListBuffer[A]
			for(v <- i) b ++= v
			b.toList
		}
	}

	implicit object StringProps extends MonoidWithJoin[String] {
		def join(a : String, b : String) = a + b
		def empty = ""
		override def concat(i : Iterable[String]) = i.mkString
	}

	implicit def ordIterableExtras[A](i : Iterable[A])(implicit ord : Ord[A]) =
		new OrdIterableExtras[A](i)(ord)

	implicit def monoidPlusIterableExtras[A](i : Iterable[A])(implicit monoid : MonoidWithPlus[A]) =
		new MonoidWithPlusIterableExtras[A](i)(monoid)

	implicit def monoidJoinIterableExtras[A](i : Iterable[A])(implicit monoid : MonoidWithJoin[A]) =
		new MonoidWithJoinIterableExtras[A](i)(monoid)
}
