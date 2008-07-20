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

package scalax.reflect

import scala.reflect.Manifest

/**
 * Convenience class to avoid working with manifests directly.
 *
 * @author Christos KK Loverdos
 */
sealed class typeOf[T](implicit val manifest: Manifest[T]) {

    /* Uncomment these when Scalax uses > 2.7.1
  def erasure = manifest.erasure

  def <:<[S](other: typeOf[S]) = manifest <:< other.manifest
  def >:>[S](other: typeOf[S]) = manifest >:> other.manifest
    */

  override def toString = manifest.toString
  override def hashCode = manifest.hashCode

  override def equals(other: Any) = other.isInstanceOf[typeOf[_]] && manifest == other.asInstanceOf[typeOf[_]].manifest
}

/**
 * Companion object providing utility methods to easily obtain a <code>typeOf</code>
 * instance either directly from a Scala type or from an object value.
 *
 * For example, try the following:
 * <blockquote>
 * <ul>
 * 	<li> typeOf[Int]
 * 	<li> typeOf(1)
 * 	<li> typeOf[Int] == typeOf(1)
 * 	<li> typeOf[List[List[Int]]].toString
 * </ul>
 * </blockquote>
 *
 * @author Christos KK Loverdos
 */
object typeOf {
  def apply[T](t: T)(implicit m: Manifest[T]) = new typeOf[T]
  def apply[T]()(implicit m: Manifest[T]) = new typeOf[T]
}

