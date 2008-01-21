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

/** A more efficient re-implementation of ArrayBuffer. */
class FastArrayBuffer[A] extends RandomAccessSeq.Mutable[A] with Buffer[A] {
	private var arr = new Array[AnyRef](4)
	private var len = 0
	private def scale(n : Int) = {
		var s = arr.length
		if(s < n) {
			while(s < n) s *= 2
			val na = new Array[AnyRef](s)
			System.arraycopy(arr, 0, na, 0, len)
			arr = na
		}
	}

	def length = len
	def apply(i : Int) = {
		if(i < 0 || i >= len) throw new IndexOutOfBoundsException(i.toString)
		arr(i).asInstanceOf[A]
	}
	def update(i : Int, x : A) = {
		if(i < 0 || i >= len) throw new IndexOutOfBoundsException(i.toString)
		arr(i) = x.asInstanceOf[AnyRef]
	}
	override def elements =
		new Iterator[A] {
			var i = 0
			def hasNext = i < len
			def next = {
				val r = arr(i).asInstanceOf[A]
				i += 1
				r
			}
		}
	def +=(x : A) = {
		scale(len + 1)
		arr(len) = x.asInstanceOf[AnyRef]
		len += 1
	}
	override def ++=(iter : Iterable[A]) = iter match {
		case that : FastArrayBuffer[_] =>
			scale(len + that.len)
			System.arraycopy(that.arr, 0, arr, len, that.len)
			len += that.len
		case _ => iter.copyToBuffer(this)
	}
	override def ++=(src : Array[A], s : Int, n : Int) {
		scale(len + n)
		Array.copy(src, s, arr, len, n)
		len += n
	}
	override def ++[B >: A](iter : Iterable[B]) = {
		val t = clone().asInstanceOf[FastArrayBuffer[B]]
		t ++= iter
		t
	}
	def +:(e : A) = {
		scale(len + 1)
		System.arraycopy(arr, 0, arr, 1, len)
		arr(0) = e.asInstanceOf[AnyRef]
		len += 1
		this
	}
	def insertAll(i : Int, iter : Iterable[A]) = {
		if(i < 0 || i > len) throw new IndexOutOfBoundsException(i.toString)
		val na = new FastArrayBuffer[A]
		na ++= iter
		val n = na.length
		scale(len + n)
		if(i < len) System.arraycopy(arr, i, arr, i + n, len - i)
		System.arraycopy(na.arr, 0, arr, i, n)
		len += n
	}
	def remove(i : Int) = {
		val r = apply(i)
		if(i < len - 1) System.arraycopy(arr, i + 1, arr, i, len - i - 1)
		len -= 1
		r
	}
	def clear() = {
		len = 0
	}
	override def clone() = {
		val nb = new FastArrayBuffer[A]
		val na = new Array[AnyRef](arr.length)
		System.arraycopy(arr, 0, na, 0, len)
		nb.arr = na
		nb.len = len
		nb
	}
}
