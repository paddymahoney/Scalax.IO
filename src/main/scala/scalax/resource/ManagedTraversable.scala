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

/** This trait provides a means to ensure traversable access to items inside a resource, while ensuring that the
 * resource is opened/closed appropriately before/after the traversal.   */
trait ManagedTraversable[+A] extends collection.Traversable[A] {
  /** The type of the handle returned when opening the resource */
  type Handle
  /**
   * The resource we plan to traverse through  
   */
  val resource : ManagedResource[Handle]

  /**
   * This method gives us an iterator over items in a resource.                               
   */
  protected def iterator(v : Handle) : Iterator[A]


  /**
   * Executes a given function against all items in the resource.  The resource is opened/closed during the call
   * to this method.
   */
  def foreach[U](f: A => U): Unit = resource.flatMap( x => iterator(x).foreach(f) )

}
