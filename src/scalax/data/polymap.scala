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

package scalax.data
import scala.collection.mutable._

/** A Map variant that operates over polymorphic element types. */
trait PolyMap[Key[_], Value[_]]
		extends Collection[(Key[A], Value[A]) forSome { type A }] {
	type ElemType = (Key[A], Value[A]) forSome { type A }
	def get[A](k : Key[A]) : Option[Value[A]]
	def apply[A](k : Key[A]) : Value[A]
	def contains[A](k : Key[A]) : Boolean
	def -=[A](k : Key[A]) : Unit
}

/** A HashMap variant that operates over polymorphic element types.
 * WARNING: This is currently sound only if Key and Value are invariant in
 * their parameters, due to compiler issue #285. */
class PolyHashMap[Key[_], Value[_]] extends PolyMap[Key, Value] {
	private val m = new HashMap[Any, Any]

	def get[A](k : Key[A]) : Option[Value[A]] =
		m.get(k).asInstanceOf[Option[Value[A]]]

	def apply[A](k : Key[A]) : Value[A] =
		m(k).asInstanceOf[Value[A]]

	def update[A](k : Key[A], v : Value[A]) : Unit =
		m(k) = v

	def contains[A](k : Key[A]) : Boolean =
		m.contains(k)

	def -=[A](k : Key[A]) : Unit =
		m -= k

	def clear() : Unit =
		m.clear()

	def elements : Iterator[ElemType] =
		m.elements.asInstanceOf[Iterator[ElemType]]

	def size : Int =
		m.size
}
