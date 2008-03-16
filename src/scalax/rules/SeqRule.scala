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

class SeqRule[S, +A, +X](rule : Rule[S, S, A, X]) {
  import rule.factory._

  def ? = from[S] { 
    in => rule(in) match {
      case Success(out, a) => Success(out, Some(a))
      case _ => Success(in, None)
    }
  }

  /** Creates a rule that always succeeds with a Boolean value.  
   *  Value is 'true' if this rule succeeds, 'false' otherwise */
  def -? = ? map { _ isDefined }
        
  def * = from[S] { 
    // tail-recursive function with reverse list accumulator
    def rep(in : S, acc : List[A]) : (S, List[A]) = rule(in) match {
       case Success(out, a) => rep(out, a :: acc)
       case _ => (in, acc)
    }
    in => rep(in, Nil) match { case (out, list) => Success(out, list.reverse) }
  }
  
  def + = rule ~++ *
    
  def ~>*[B >: A, X2 >: X](f : => Rule[S, S, B => B, X2]) = for (a <- rule; fs <- f*) yield fs.foldLeft[B](a) { (b, f) => f(b) }
    
  def ~*~[B >: A, X2 >: X](join : => Rule[S, S, (B, B) => B, X2]) = {
    this ~>* (for (f <- join; a <- rule) yield f(_ : B, a))
  }
  
  /** Repeats this rule one or more times with a separator (which is discarded) */
  def +/(sep : => Rule[S, S, Any, Any]) = rule ~++ (sep -~ rule *)

  /** Repeats this rule zero or more times with a separator (which is discarded) */
  def */(sep : => Rule[S, S, Any, Any]) = +/(sep) | state[S].nil
  
  def *~-[Out, Y](end : => Rule[S, Out, Any, Y]) = (rule - end *) ~- end
  def +~-[Out, X2 >: X](end : => Rule[S, Out, Any, X2]) = (rule - end +) ~- end

}

