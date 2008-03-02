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
import scala.collection.mutable._
import scala.collection.jcl.MapWrapper
import java.util.concurrent.atomic._
import java.util.concurrent.{ConcurrentHashMap => JConcurrentHashMap, _}

final class AtomicCell[A](init : A) {
	private val ref = new AtomicReference(init)
	def compareAndSet(expect : A, update : A) : Boolean =
		ref.compareAndSet(expect, update)
	def get : A = ref.get().asInstanceOf[A]
	def getAndSet(n : A) : A = ref.getAndSet(n).asInstanceOf[A]
	def set(n : A) : Unit = ref.set(n)
	override def toString() : String = ref.get.toString
	def weakCompareAndSet(expect : A, update : A) : Boolean =
		ref.weakCompareAndSet(expect, update)
}

class ConcurrentHashMap[K,V](val jmap : JConcurrentHashMap[K,V]) extends MapWrapper[K,V] {
	def underlying = jmap
	def this() = this(new JConcurrentHashMap(2))
	def putIfAbsent(k : K, v : V) : Option[V] = {
		val r = jmap.putIfAbsent(k, v)
		if(r == null) None else Some(r)
	}
	def remove(k : K, v : V) : Boolean = jmap.remove(k, v)
	def replace(k : K, o : V, n : V) : Boolean = jmap.replace(k, o, n)
	def replace(k : K, v : V) : V = jmap.replace(k, v)
}

class ConcurrentHashSet[A](val jmap : JConcurrentHashMap[A, List[Any]]) extends Set[A] {
	def this() = this(new JConcurrentHashMap(2))
	def +=(elem : A) : Unit = jmap.put(elem, Nil)
	def -=(elem : A) : Unit = jmap.remove(elem)
	override def clear : Unit = jmap.clear()
	override def clone : Set[A] = {
		val n = new ConcurrentHashSet[A]
		n ++= this
		n
	}
	def contains(elem : A) : Boolean = jmap.containsKey(elem)
	def elements : Iterator[A] = {
		val i = jmap.keySet.iterator
		new Iterator[A] {
			def hasNext = i.hasNext
			def next = i.next()
		}
	}
	override def isEmpty : Boolean = jmap.isEmpty
	def size : Int = jmap.size
	def testAndAdd(elem : A) : Boolean = {
		val r = jmap.put(elem, Nil)
		r != null
	}
	def testAndRemove(elem : A) : Boolean = {
		val r = jmap.remove(elem)
		r != null
	}
}

class ConcurrentQueue[A](val jq : LinkedBlockingQueue[A]) {
	def this() = this(new java.util.concurrent.LinkedBlockingQueue)
	def +=(elem : A) : Unit = jq.put(elem)
	def clear() : Unit = jq.clear()
	def poll() : Option[A] = {
		val r = jq.poll()
		if(r == null) None else Some(r)
	}
	def poll(millis : Long) = {
		val r = jq.poll(millis, java.util.concurrent.TimeUnit.MILLISECONDS)
		if(r == null) None else Some(r)
	}
	def size : Int = jq.size
	def take() : A = jq.take().asInstanceOf[A]
}

/** A thread-safe linked list implementation in which all operations except
 * '-=' are unsynchronized. */
class ConcurrentLinkedList[A] extends Seq[A] {
	private trait Node {
		def elem : A
		def isSentinel : Boolean
		val next : AtomicCell[Node]
	}

	private class SentinelNode extends Node {
		def elem = throw new Exception("Element not found")
		def isSentinel = true
		val prev = new AtomicCell[Node](this)
		val next = new AtomicCell[Node](this)
	}

	private val sentinel = new AtomicCell(new SentinelNode)

	private class DataNode(val elem : A, val next : AtomicCell[Node]) extends Node {
		def isSentinel = false
	}

	def elements : Iterator[A] = new Iterator[A] {
		var curr = sentinel.get.next.get
		def hasNext = !curr.isSentinel
		def next = {
			val r = curr.elem
			curr = curr.next.get
			r
		}
	}

	/** Appends an element. Appended elements are guaranteed to appear in the
	 * list eventually, in the order that they were added, but an iteration
	 * immediately following an append is not guaranteed to return the new
	 * element yet. */
	def +=(elem : A) : Unit = {
		val s = sentinel.get
		val n = new DataNode(elem, new AtomicCell(s))
		var prev = s.prev.getAndSet(n)
		prev.next.set(n)
	}

	/** This operation is not generally useful, because there is never any
	 * guarantee that the result is current. */
	def length : Int = {
		var count = 0
		var curr = sentinel.get.next.get
		while(!curr.isSentinel) {
			count += 1
			curr = curr.next.get
		}
		count
	}

	def apply(n : Int) : A = {
		val i = elements
		i.drop(n)
		i.next
	}

	/** Deletes the first matching node. This is safe w.r.t. concurrent appends
	 * and iterations, but is synchronized to protect it from itself. */
	def -=(elem : A) : Unit = synchronized {
		var prev : Node = sentinel.get
		var curr = prev.next.get
		while(!curr.isSentinel) {
			if(curr.elem == elem) {
				prev.next.set(curr.next.get)
				curr = sentinel.get
			} else {
				prev = curr
				curr = curr.next.get
			}
		}
	}

	def clear() : Unit = {
		sentinel.set(new SentinelNode)
	}
}
