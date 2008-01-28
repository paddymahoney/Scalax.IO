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

package scalax.rules

trait Input[+A, Context <: Input[A, Context]] extends Iterable[A] { self : Context =>

  def next : Result[(A, Context)]
  def index : Int

  def elements = new Iterator[A] {
    private var input : Context = Input.this
    private var result = input.next

    def hasNext = result != Failure
    def next = {
      val Success(value, input) = result
      this.input = input
      this.result = input.next
      value
    }
  }
}


class ArrayInput[A](val array : Array[A], val index : Int) extends Input[A, ArrayInput[A]] {
  def this(array : Array[A]) = this(array, 0)

  lazy val next = if (index >= array.length) Failure
      else Success(array(index), new ArrayInput[A](array, index + 1))
 
  override lazy val toString = elements.mkString("\"", "", "\"")
}


class IterableInput[A](iterator : Iterator[A], val index : Int) extends Input[A, IterableInput[A]] {
  def this(iterable : Iterable[A]) = this(iterable.elements, 0)

  lazy val next = if (!iterator.hasNext) Failure
      else Success(iterator.next, new IterableInput(iterator, index + 1))

  override lazy val toString = elements.mkString("\"", "", "\"")
}


/** View one type of input as another based on a transformation rule */
class View[A, B, Context <: Input[A, Context]](
    transform : Context => Result[(B, Context)],
    val input : Context,
    val index : Int)
    extends Input[B, View[A, B, Context]] {

  def next = transform(input) match {
    case Success(b, context) => Success(b, new View(transform, context, index + 1))
    case _ => Failure
  }
}
