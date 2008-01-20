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

package scalax.io
import java.sql._

/** Functions which wrap various Java-style generators into Iterators which
 * can be used within scala 'for' comprehensions. */
object IteratorHelp {
	/** Keeps running the block until it returns null. */
	def nonNull[A <: AnyRef](f : => A) =
		new Iterator[A] {
			private var n : A = _
			private var h = false

			def hasNext = {
				if(!h) {
					n = f
					h = true
				}
				n != null
			}

			def next = {
				if(!h) n = f
				h = false
				n
			}
		}

	/** Keeps running the block until it returns a negative number. */
	def nonNegative(f : => Int) =
		new Iterator[Int] {
			private var n = 0
			private var h = false

			def hasNext = {
				if(!h) {
					n = f
					h = true
				}
				n >= 0
			}

			def next = {
				if(!h) n = f
				h = false
				n
			}
		}

	/** Wraps a Java iterator into a Scala one. */
	def fromJava[A](i : java.util.Iterator) =
		new Iterator[A] {
			def hasNext = i.hasNext()
			def next = i.next().asInstanceOf[A]
		}

	/** Returns the block value for each row of a ResultSet. */
	def resultSet[A](rs : ResultSet)(f : => A) =
		new Iterator[A] {
			private var h = rs.next()

			def hasNext = h

			def next = {
				val r = f
				h = rs.next()
				r
			}
		}

	/** Applies the supplied function to each value and returns the first Some
	 * result, if there is one. */
	def findResult[A, B](iter : Iterator[A], f : A => Option[B]) : Option[B] = {
		var res : Option[B] = None
		while((res eq None) && iter.hasNext) {
			res = f(iter.next)
		}
		res
	}
}
