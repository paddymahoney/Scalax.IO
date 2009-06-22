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

package scalax.resource

/** Provides automatic resource management, equivalent to C#'s using, or C++
 * RAII/RRID. Idiomatic usage would be as follows (in fact FileExtras provides a
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
trait ManagedResource[+A] { self =>
	type Handle

	/** Should be implemented to acquire the managed resource. Clients are
	 * discouraged from calling this method directly, as the returned resource
	 * must be disposed of manually. */
	protected def unsafeOpen() : Handle

	/** Should be implemented to dispose of the managed resource. This will be
	 * called automatically when the ManagedResource is used in a
	 * for-comprehension. */
	protected def unsafeClose(v : Handle) : Unit
	
	protected def unsafeCloseQuietly(v : Handle) {
		try {
			unsafeClose(v)
		} catch {
			case e => e.printStackTrace()
		}
	}
	
	/** Close ignoring Exception, but not any Throwable. */
	protected def unsafeCloseIgnoringException(v : Handle) {
		try {
			unsafeClose(v)
		} catch {
			case e: Exception => e.printStackTrace()
		}
	}

	/** Should be implemented to translate a Handle into the desired resource
	 * type. */
	protected def translate(v : Handle) : A

	def foreach(f : A => Unit) : Unit = acquireFor(f)
	def flatMap[B](f : A => B) : B = acquireFor(f)
	def map[B](f : A => B) : B = acquireFor(f)

	/** Acquires the resource for the duration of the supplied function. */
	def acquireFor[B](f : A => B) : B = {
		val v = unsafeOpen()
                var exception = true  /*Mutability YUK! - at least we wont try to close if we've already closed*/
		try {
			val r = f(translate(v))
			unsafeClose(v)
                        exception = false
			r
		} finally {
                        if(exception) 
 			   unsafeCloseQuietly(v)
		}
	}	
}

/** The typical case of a ManagedResource, where the handle and resource are
 * the same object. */
trait UntranslatedManagedResource[A] extends ManagedResource[A] {
	type Handle = A
	final def translate(v : Handle) = v
}

object ManagedResource {
	/** Creates a ManagedResource for any type with a close method. Note that
	 * the opener argument is evaluated on demand, possibly more than once, so
	 * it must contain the code that actually acquires the resource. Clients
	 * are encouraged to write specialized methods to instantiate
	 * ManagedResources rather than relying on ad-hoc usage of this method. */
	def apply[A <: { def close() : Unit }](opener : => A) =
		new UntranslatedManagedResource[A] {
			def unsafeOpen() = opener
			def unsafeClose(r : A) = r.close()
	}
}
