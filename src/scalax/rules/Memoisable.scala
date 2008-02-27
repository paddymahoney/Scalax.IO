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

import scala.collection.mutable.HashMap

trait MemoisableRules extends Rules {
  type S <: Memoisable[S]
  
  override def createRule[In, Out, A, X](name : String, f : In => rules.Result[Out, A, X]) = super.createRule(name, (in : In) => in match {
      case s : Memoisable[In] => s.memo(name, f)
      case _ => f(in)
    })
  
  //def memo[A](key : AnyRef, f : S => Result[A]) : Rule[A] = createRule(key.toString, f)
}

trait Memoisable[Context] {
  def memo[A](key : AnyRef, f : Context => A) : A
}


object DefaultMemoisable {
  var debug = false
}

trait DefaultMemoisable[Context <: Memoisable[Context]] extends Memoisable[Context] {
  self : Context =>

  protected val map = new HashMap[AnyRef, Any]

  def memo[A](key : AnyRef, f : Context => A) = {
    map.getOrElseUpdate(key, compute(key, f)).asInstanceOf[A]
  }
  
  protected def compute[A](key : AnyRef, f : Context => A) = f(this) match {
    case success : Success[Context, _] => onSuccess(key, success); success
    case other => other
  }
  
  protected def onSuccess[T](key : AnyRef,  result : Success[Context, T])  { 
    val Success(out, t) = result
    if(DefaultMemoisable.debug) println(key + " -> " + t + " (" + out + ")") 
  }
}



