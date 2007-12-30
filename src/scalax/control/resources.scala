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
import java.io._

/** Provides automatic resource management, equivalent to C#'s using, or C++
 * RAII. Idiomatic usage would be as follows (in fact FileExtras provides a
 * method equivalent to fileReader below):
 *
 * <pre>
 * def fileReader(f : String) = ManagedResource(new FileReader(f))
 *
 * // Print the first character of a file
 * for(in &lt;- fileReader("/etc/passwd"))
 *     println(in.read().toChar)
 * </pre>
 */
abstract class ManagedResource[A] {
	/** Should be implemented to acquire the managed resource. Clients are
	 * discouraged from calling this method directly, as the returned resource
	 * must be disposed of manually. */
	def open() : A

	/** Should be implemented to dispose of the managed resource. This will be
	 * called automatically when the ManagedResource is used in a
	 * for-comprehension. */
	def close(v : A) : Unit

	def foreach(f : A => Unit) : Unit = map(f)
	def flatMap[B](f : A => B) : B = map(f)
	def map[B](f : A => B) : B = {
		val v = open()
		// This is the same as ensuring, copied here to avoid creating
		// two more closures.
		val r =
			try {
				f(v)
			} catch {
				case e : Throwable =>
					try {
						close(v)
					} catch {
						// Ignore secondary exception
						case e2 : Exception => ()
					}
					throw e
			}
		close(v)
		r
	}
}

object ManagedResource {
	/** Creates a ManagedResource for any type with a close method. Note that
	 * the opener argument is evaluated on demand, possibly more than once, so
	 * it must contain the code that actually acquires the resource. Clients
	 * are encouraged to write specialized methods to instantiate
	 * ManagedResources rather than relying on ad-hoc usage of this method. */
	def apply[A <: { def close() : Unit }](opener : => A) =
		new ManagedResource[A] {
			def open() = opener
			def close(r : A) = r.close()
		}
}
