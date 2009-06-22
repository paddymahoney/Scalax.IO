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

//TODO - Ensure all Iterable methods are available *except* iterator/elements

/** A trait which is approximately isomorphic to Iterable, but which manages the
 * lifecycle of the underlying data source, and is generally non-strict. */
trait ManagedTraversible[+A] extends collection.Traversable[A] { self =>
  type Handle
  val resource : ManagedResource[Handle]
  // N.B. these functions are permitted to trash the handle
  protected def iterator(v : Handle) : Iterator[A]



  def foreach[U](f: A => U): Unit = for(v <- resource) {
      val i = iterator(v)
      while(i.hasNext) {
          f(i.next)
      }
  }


  /*


  // Abstract trait to use when lazily mapping or flatMapping
  private trait Child[+B] extends ManagedTraversible[B] {
		type Handle = self.Handle
		val resource = self.resource
  }

  def map[B](f: A => B): ManagedIterable[B] = new Child[B] {
     protected def iterator(v : Handle) = self.iterator(v).map(f)
  }

  def flatMap[B](f : A => Iterable[B]) : ManagedIterable[B] = new Child[B] {
     protected def iterator(v : Handle) = self.iterator(v).flatMap(x => f(x).elements)
  }
*/


}
