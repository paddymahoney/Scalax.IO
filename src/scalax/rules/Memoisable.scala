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

import _root_.scala.collection.mutable.HashMap

trait MemoisableRules extends Rules {
  type S <: Memoisable[S]
  
  def memo[A](key : AnyRef, f : S => Result[(A, S)]) : Rule[A] = rule[A] { ctx => ctx.memo(key, f) }
}

trait Memoisable[Context] {
  def memo[A](key : AnyRef, f : Context => Result[(A, Context)]) : Result[(A, Context)]
}

trait DefaultMemoisable[Context <: Memoisable[Context]] extends Memoisable[Context] {
  
  self : Context =>

  protected val map = new HashMap[AnyRef, Result[(Any, Context)]]

  def memo[T](key : AnyRef, f : Context => Result[(T, Context)]) = {
    map.getOrElseUpdate(key, compute(key, f)).asInstanceOf[Result[(T, Context)]]
  }
  
  protected def compute[T](key : AnyRef, f : Context => Result[(T, Context)]) = f(this) match {
    case success @ Success((value, context)) => onSuccess(key, success); success
    case failure => failure
  }
  
  protected def onSuccess[T](key : AnyRef,  result : Success[(T, Context)]) { }
}



