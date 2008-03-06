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

package scalax.control
import java.io._
import scala.collection.mutable._
import scalax.data._

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
abstract class ManagedResource[+A] { self =>
	type Handle

	/** Should be implemented to acquire the managed resource. Clients are
	 * discouraged from calling this method directly, as the returned resource
	 * must be disposed of manually. */
	def unsafeOpen() : Handle

	/** Should be implemented to dispose of the managed resource. This will be
	 * called automatically when the ManagedResource is used in a
	 * for-comprehension. */
	def unsafeClose(v : Handle) : Unit
	
	def unsafeCloseQuietly(v : Handle) {
		try {
			unsafeClose(v)
		} catch {
			case e => e.printStackTrace()
		}
	}

	/** Should be implemented to translate a Handle into the desired resource
	 * type. */
	def translate(v : Handle) : A

	def foreach(f : A => Unit) : Unit = acquireFor(f)
	def flatMap[B](f : A => B) : B = acquireFor(f)
	def map[B](f : A => B) : B = acquireFor(f)

	/** Acquires the resource for the duration of the supplied function. */
	def acquireFor[B](f : A => B) : B = {
		val v = unsafeOpen()
		try {
			val r = f(translate(v))
			unsafeClose(v)
			r
		} finally {
			unsafeCloseQuietly(v)
		}
	}

	def and[B](that : ManagedResource[B]) : ManagedResource[(A, B)] =
		new ManagedResource[(A, B)] {
			type Handle = (self.Handle, that.Handle)
			def unsafeOpen() = {
				val a = self.unsafeOpen()
				val b =
					try {
						that.unsafeOpen()
					} catch {
						case e =>
							self.unsafeClose(a)
							throw e
					}
				(a, b)
			}
			def unsafeClose(v : Handle) =
				try {
					that.unsafeClose(v._2)
				} finally {
					self.unsafeClose(v._1)
				}
			def translate(v : Handle) =
				(self.translate(v._1), that.translate(v._2))
		}
}

/** The typical case of a ManagedResource, where the handle and resource are
 * the same object. */
abstract class UntranslatedManagedResource[A] extends ManagedResource[A] {
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

/** A class which is approximately isomorphic to Seq, but which manages the
 * lifecycle of the underlying data source, and is generally non-strict. */
abstract class ManagedSequence[+A] extends PartialFunction[Int, A] { self =>
	type Handle
	val resource : ManagedResource[Handle]
	// N.B. these functions are permitted to trash the handle
	protected def iterator(v : Handle) : Iterator[A]
	protected def length(v : Handle) : Int = {
		var i = 0
		val iter = iterator(v)
		while(iter.hasNext) {
			iter.next
			i += 1
		}
		i
	}
	protected def apply(v : Handle, i : Int) : A =
		iterator(v).drop(i).next

	private abstract class Child[+B] extends ManagedSequence[B] {
		type Handle = self.Handle
		val resource = self.resource
	}

	def elements = new ManagedResource[Iterator[A]] {
		type Handle = resource.Handle
		def unsafeOpen() = resource.unsafeOpen()
		def unsafeClose(v : Handle) = resource.unsafeClose(v)
		def translate(v : Handle) = iterator(resource.translate(v))
	}
	def length : Int = for(v <- resource) yield length(v)
	def isEmpty = for(v <- resource) yield iterator(v).hasNext
	def apply(i : Int) : A = for(v <- resource) yield apply(v, i)
	def last = lastOption.get
	def lastOption =
		for(v <- resource) yield {
			val iter = iterator(v)
			if(iter.hasNext) {
				var x = iter.next
				while(iter.hasNext) x = iter.next
				Some(x)
			} else {
				None
			}
		}
	def head = apply(0)
	def headOption =
		for(v <- resource) yield {
			val iter = iterator(v)
			if(iter.hasNext) Some(iter.next) else None
		}
	def ++[B >: A](that : ManagedSequence[B]) : ManagedSequence[B] =
		new ManagedSequence[B] {
			type Handle = (self.Handle, that.Handle)
			val resource = self.resource and that.resource
			protected def iterator(v : Handle) =
				self.iterator(v._1) ++ that.iterator(v._2)
			protected override def length(v : Handle) =
				self.length(v._1) + that.length(v._2)
			protected override def apply(v : Handle, i : Int) : B = {
				if(i < 0) throw new NoSuchElementException
				var j = 0
				val iter = self.iterator(v._1)
				while(iter.hasNext) {
					val x = iter.next
					if(i == j) return x
					j += 1
				}
				that(v._2, i - j)
			}
		}
	def ++[B >: A](that : Seq[B]) : ManagedSequence[B] =
		new Child[B] {
			protected def iterator(v : Handle) =
				self.iterator(v) ++ that.elements
			protected override def length(v : Handle) =
				self.length(v) + that.length
			protected override def apply(v : Handle, i : Int) : B = {
				if(i < 0) throw new NoSuchElementException
				var j = 0
				val iter = self.iterator(v)
				while(iter.hasNext) {
					val x = iter.next
					if(i == j) return x
					j += 1
				}
				that(i - j)
			}
		}
	def isDefinedAt(i : Int) = i < length
	def lastIndexOf(e : Any) = {
		for(r <- resource) yield {
			val iter = iterator(r)
			var i = 0
			var pos = -1
			while(iter.hasNext) {
				if(iter.next == e) pos = i
				i += 1
			}
			pos
		}
	}
	def map[B](f : A => B) : ManagedSequence[B] =
		new Child[B] {
			protected def iterator(v : Handle) = self.iterator(v).map(f)
			protected override def length(v : Handle) = self.length(v)
			protected override def apply(v : Handle, i : Int) = f(self.apply(v, i))
		}
	def flatMap[B](f : A => Iterable[B]) : ManagedSequence[B] =
		new Child[B] {
			protected def iterator(v : Handle) =
				self.iterator(v).flatMap(x => f(x).elements)
		}
	def filter(f : A => Boolean) : ManagedSequence[A] =
		new Child[A] {
			protected def iterator(v : Handle) = self.iterator(v).filter(f)
		}
	def take(n : Int) : ManagedSequence[A] =
		new Child[A] {
			protected def iterator(v : Handle) = self.iterator(v).take(n)
			protected override def length(v : Handle) = {
				val len = self.length(v)
				if(len < n) len else n
			}
			protected override def apply(v : Handle, i : Int) =
				if(i < n) self.apply(v, i)
				else throw new NoSuchElementException
		}
	def drop(n : Int) : ManagedSequence[A] =
		new Child[A] {
			protected def iterator(v : Handle) = self.iterator(v).drop(n)
			protected override def length(v : Handle) = {
				val len = self.length(v)
				if(len < n) 0 else len - n
			}
			protected override def apply(v : Handle, i : Int) =
				self.apply(v, i + n)
		}
	def slice(from : Int, until : Int) : ManagedSequence[A] =
		new Child[A] {
			protected def iterator(v : Handle) =
				self.iterator(v).drop(from).take(until - from)
			protected override def length(v : Handle) = {
				val len = self.length(v)
				if(until < len) until - from else len - from
			}
			protected override def apply(v : Handle, i : Int) =
				if(i + from < until) self.apply(v, i + from)
				else throw new NoSuchElementException
		}
	def takeWhile(f : A => Boolean) : ManagedSequence[A] =
		new Child[A] {
			protected def iterator(v : Handle) = self.iterator(v).takeWhile(f)
		}
	def dropWhile(f : A => Boolean) : ManagedSequence[A] =
		new Child[A] {
			protected def iterator(v : Handle) = self.iterator(v).dropWhile(f)
		}
	def reverse =
		for(v <- resource) yield {
			var xs : List[A] = Nil
			val iter = iterator(v)
			while(iter.hasNext) {
				xs = iter.next :: xs
			}
			xs
		}
	def contains(e : Any) =
		for(v <- resource) yield {
			iterator(v).contains(e)
		}
	def toArray[B >: A] =
		for(v <- resource) yield {
			val a = new FastArrayBuffer[B]
			a ++= iterator(v)
			a.toArray
		}
	def startsWith(that : Seq[Any]) =
		for(v <- resource) yield {
			val iter = iterator(v)
			val jter = that.elements
			var r = true
			while(iter.hasNext && jter.hasNext)
				if(iter.next != jter.next) r = false
			r && !jter.hasNext
		}
	def endsWith(that : Seq[Any]) = {
		val ilen = for(v <- resource) yield length(v)
		val jlen = that.length
		if(jlen > ilen) {
			false
		} else {
			for(v <- resource) yield {
				val iter = iterator(v)
				iter.drop(ilen - jlen)
				val jter = that.elements
				var r = true
				while(iter.hasNext && jter.hasNext)
					if(iter.next != jter.next) r = false
				r && !iter.hasNext && !jter.hasNext
			}
		}
	}
	def indexOf(that : Seq[Any]) =
		for(v <- resource) yield {
			val iter = iterator(v)
			var i = 0
			var jter = that.elements
			var j = -1
			while(iter.hasNext && jter.hasNext) {
				if(iter.next != jter.next) {
					if(j != -1) {
						jter = that.elements
						j = -1
					}
				} else if(j == -1) {
					j = i
				}
				i += 1
			}
			j
		}
	def containsSlice(that : Seq[Any]) = indexOf(that) != -1
	def foreach(f : A => Unit) = for(v <- resource) yield iterator(v).foreach(f)
	def forall(f : A => Boolean) = for(v <- resource) yield iterator(v).forall(f)
	def exists(f : A => Boolean) = for(v <- resource) yield iterator(v).exists(f)
	def find(f : A => Boolean) = for(v <- resource) yield iterator(v).find(f)
	def findIndexOf(f : A => Boolean) =
		for(v <- resource) yield {
			var i = 0
			var r = -1
			val iter = iterator(v)
			while(r == -1 && iter.hasNext) {
				if(f(iter.next)) r = i
				i += 1
			}
			r
		}
	def indexOf(e : Any) =
		for(v <- resource) yield {
			var i = 0
			var r = -1
			val iter = iterator(v)
			while(r == -1 && iter.hasNext) {
				if(iter.next == e) r = i
				i += 1
			}
			r
		}
	def foldLeft[B](z : B)(op : (B, A) => B) =
		for(v <- resource) yield iterator(v).foldLeft(z)(op)
	def foldRight[B](z : B)(op : (A, B) => B) =
		for(v <- resource) yield iterator(v).foldRight(z)(op)
	def reduceLeft[B >: A](op : (B, B) => B) =
		for(v <- resource) yield iterator(v).reduceLeft(op)
	def reduceRight[B >: A](op : (B, B) => B) =
		for(v <- resource) yield iterator(v).reduceRight(op)
	def copyToBuffer[B >: A](buf : Buffer[B]) =
		for(v <- resource) buf ++= iterator(v)
	def sameElements(that : Iterable[Any]) =
		for(v <- resource) yield {
			val iter = iterator(v)
			val jter = that.elements
			var r = true
			while(iter.hasNext && jter.hasNext)
				if(iter.next != jter.next) r = false
			r && !iter.hasNext && !jter.hasNext
		}
	def sameElements(that : ManagedSequence[Any]) =
		for(v <- resource; w <- that.resource) yield {
			val iter = iterator(v)
			val jter = that.iterator(w)
			var r = true
			while(iter.hasNext && jter.hasNext)
				if(iter.next != jter.next) r = false
			r && !iter.hasNext && !jter.hasNext
		}
	def toList =
		for(v <- resource) yield iterator(v).toList
	def mkString(start : String, sep : String, end : String) : String =
		for(v <- resource) yield {
			val iter = iterator(v)
			val sb = new StringBuffer
			sb.append(start)
			if(iter.hasNext) sb.append(iter.next.toString)
			while(iter.hasNext) {
				sb.append(sep)
				sb.append(iter.next.toString)
			}
			sb.append(end)
			sb.toString
		}
	def mkString(sep : String) : String = mkString("", sep, "")
	def mkString : String = mkString("", "", "")
	def copyToArray[B >: A](arr : Array[B], start : Int) =
		for(v <- resource) yield iterator(v).copyToArray(arr, start)
	protected def stringPrefix = "ManagedSequence"
	override def toString = mkString(stringPrefix+"(", ", ", ")")
}
