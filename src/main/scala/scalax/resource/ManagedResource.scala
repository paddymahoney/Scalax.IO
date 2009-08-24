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

/**
 * This trait represents the abstract methods needed to be implemented to create a ManagedResource. 
 */
abstract trait ManagedResourceOps[+A] {
   /** The handle returned when opening this managed resource.  The handle allows us to close the resource when finished */
   type Handle
     /** Implement to acquire the managed resource.  Clients should not
      *  call this directly, as the returned resource will need to be
      *  disposed of manually.
      */
     protected def unsafeOpen(): Handle
     
     /** Implement to dispose of the managed resource.  Called automatically
      *  when the ManagedResource is used in a for comprehension.
      */
     protected def unsafeClose(v: Handle): Unit
      
     /** Implement to translate a Handle into the desired resource type. */
     protected def translate(v: Handle): A

}


/** Provides automatic resource management, equivalent to C#'s using, or C++
 * RAII/RRID. Idiomatic usage would be as follows:
 *
 * <pre>
 * def fileReader(f : String) = ManagedResource(new FileReader(f))
 *
 * // Print the first character of a file
 * for(in &lt;- fileReader("/etc/passwd"))
 *     println(in.read().toChar)
 * </pre>
 */
trait ManagedResource[+A] extends ManagedResourceOps[A] { self =>
	type Handle
	
	/** Closes a resource, supressing all exceptions in the case where an exception has already 
	 * occured during the usage of the resource.  
	 * 
	 * Sub-classes should override this to ignore only the exceptions thrown during a close of the resource, or to change behavior of a "closeAfterException" path
	 */
	protected def unsafeCloseQuietly(v : Handle) {
		try {
			unsafeClose(v)
		} catch {
			case e => //TODO -Ignore specific exceptions
		}
	}	

	def foreach(f : A => Unit) : Unit = acquireFor(f)
	def flatMap[B](f : A => B) : B = acquireFor(f)

	/** Acquires the resource for the duration of the supplied function. */
	def acquireFor[B](f : A => B) : B = {
		val v = unsafeOpen()
		var okToClose = true  /*Mutability YUK! - at least we wont try to close if we've already closed*/
		try {
			val r = f(translate(v))
			unsafeClose(v)
         okToClose = false
			r
		} finally {
          if(okToClose) 
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
	/**
	 * Creates a ManagedResource for anything implementing the java.io.Closeable interface.
	 * 
	 * This method ensures that only java.io.IOException's are suppressed in the event of an exception occurring during
	 * the closing of a resource when an early exception has already occurred
	 */
	def managedCloseable[A <: java.io.Closeable](opener : => A) = new UntranslatedManagedResource[A] {
	   def unsafeOpen() = opener
	   def unsafeClose(r : A) = r.close()
	   
	   protected override def unsafeCloseQuietly(r : A) {
	      try {
	         unsafeClose(r)
	      } catch {
	         case e : java.io.IOException => //Ignore only I/O related exceptions
	      }
	   }  
	}
}
