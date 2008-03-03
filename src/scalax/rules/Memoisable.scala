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
  type S <: Memoisable
  
  override def createRule[In, Out, A, X, Err](name : String, f : In => rules.Result[Out, A, X, Err]) = super.createRule(name, (in : In) => in match {
      case s : Memoisable => s.memo(name, f(in))
      case _ => f(in)
    })
}

trait Memoisable {
  def memo[A](key : AnyRef, a : => A) : A
}


object DefaultMemoisable {
  var debug = false
}

trait DefaultMemoisable extends Memoisable {
  protected val map = new HashMap[AnyRef, Any]

  def memo[A](key : AnyRef, a : => A) = {
    map.getOrElseUpdate(key, compute(key, a)).asInstanceOf[A]
  }
  
  protected def compute[A](key : AnyRef, a : => A) = a match {
    case success : Success[_, _] => onSuccess(key, success); success
    case other => other
  }
  
  protected def onSuccess[S, T](key : AnyRef,  result : Success[S, T])  { 
    val Success(out, t) = result
    if(DefaultMemoisable.debug) println(key + " -> " + t + " (" + out + ")") 
  }
}



