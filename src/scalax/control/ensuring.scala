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

package scalax.control

object ensuring {
	/** Ensures that the given disposal function is called following execution
	 * of the body. This differs from 'finally' in that if both the body and
	 * dispose blocks throw exceptions, the one from the body will be
	 * propagated. */
	def apply[A](dispose : => Unit)(body : => A) : A = {
		val r =
			try {
				body
			} catch {
				case e : Throwable =>
					try {
						dispose
					} catch {
						// Ignore secondary exception
						case e2 : Exception => ()
					}
					throw e
			}
		dispose
		r
	}
}
