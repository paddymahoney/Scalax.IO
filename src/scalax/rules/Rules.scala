package scalax.rules

/** Represents the combined value of two rules applied in sequence.
  *
  * @see the Scala parser combinator
  */
case class ~[+A, +B](_1 : A, _2 : B)
   
/** Defines Rules that apply to a particular Context.
 * 
 * The result may be:
 * - Success, with a resulting Context and a value of some type.  Several successful rules may be applied in sequence and their values combined.
 * - Failure. A failure may result in some alternative rule being applied.
 * - Error, indicated by throwing a RuleException.  No further rules should be attempted.
 *
 * @requires Context the type of context to which rules apply.
 *
 * @author Andrew Foggin
 * @author inspired by the Scala parser combinator
 */
trait Rules extends StateReader {
  type M[+A] = Rule[A]
  
  def unit[A](a : => A) = rule[A] { s => Success(a, s) }
  override def zero = ZeroRule
  
  def get = rule[S] { s => Success(s, s) }
  def read[A](f : S => A) = rule[A] { s => Success(f(s), s) }
  def set(s : => S) = rule { oldS => Success(oldS, s) }
  def update(f : S => S) = rule { s => Success(s, f(s)) }
  
  /** Creates a Rule that always succeeds with the specified value. */
  def success[A](a : A) = unit(a)
    
  lazy val nil = success(Nil)
  lazy val none = success(None)
    
  /** Primitive rule that always suceeds and returns the context with which it is called. */
  lazy val context = get
    
  /** Create a rule that suceeds if f(context) is true.  The value returned is the context. */
  def predicate(f : S => Boolean) = for (ctx <- context if f(ctx)) yield ctx

  def toException(f : S => String) = rule[Nothing] { ctx => throw new RuleException(ctx, f(ctx)) }
  def exception(message : String) = toException { ctx => message }
    
  /** Converts a rule into a function that throws a RuleException on failure.
   */
  implicit def expect[A](rule : Rule[A]) : S => A = (context) => rule(context) match {
    case Success(a, _) => a
    case _ => throw new RuleException(context, "Unexpected failure")
  }
    
  def select[A](rules : Collection[Rule[A]]) : Rule[A] = rules.reduceLeft[Rule[A]](_ | _)

  abstract class Rule[+A] extends (S => Result[(A, S)]) with Monad[A] with OrElse[A] {
    def |[B >: A](other : => Rule[B]) = orElse(other)

    def ^^[B](f : A => B) = map(f)
    
    def ^^?[B](pf : PartialFunction[A, B]) = flatMap { a => if (pf.isDefinedAt(a)) unit(pf(a)) else zero }
    
    def -^[B](b : B) = map { any => b }
   
    def >>[B](f : A => Rule[B]) = flatMap(f)
    
    /** Recursive application */
    def >>>[B >: A <% S, C](r : Rule[C]) = for ( s <- this;  _ <- set(s); c <- r) yield c
    
    def >>?[B](pf : PartialFunction[A, Rule[B]]) = >> { a => if (pf.isDefinedAt(a)) pf(a) else zero }
    
    def ~[B](next : => Rule[B]) = for (a <- this; b <- next) yield new ~(a, b)
    
    def ~-[B](next : => Rule[B]) = for (a <- this; b <- next) yield a
    
    def -~[B](next : => Rule[B]) = for (a <- this; b <- next) yield b
    
    def ~++[B >: A](next : => Rule[Seq[B]]) = for (a <- this; b <- next) yield a :: b.toList
    
    def ? : Rule[Option[A]] = ^^ (Some(_)) | unit(None)

    def * = rule[List[A]] { c =>
      // tail-recursive function with reverse list accumulator
      def rep(acc : List[A], c : S) : (List[A], S) = apply(c) match {
         case Success(a, c) => rep(a :: acc, c)
         case _ => (acc, c)
      }
      rep(Nil, c) match { case (list, c) => Success(list.reverse, c) }
    }

    def + = this ~++ *

    def ~*~[B >: A](join : Rule[(B, B) => B]) : Rule[B] = {
      val rhs = for (f <- join; a <- this) yield f(_ : B, a)
      for (a <- this; fs <- rhs*) yield fs.foldLeft[B](a) { (b, f) => f(b) }
    }
    
    /** Repeats this rule one or more times with a separator (which is discarded) */
    def +/(sep : Rule[Any]) = this ~++ (sep -~ this *)

    /** Repeats this rule zero or more times with a separator (which is discarded) */
    def */(sep : Rule[Any]) = +/(sep) | unit(Nil)

    /** Creates a rule that suceeds only if this rule would fail on the given context. */
    def unary_! = for (s <- get if apply(s) == Failure) yield s
         
    /** Creates a rule that does not modify the state even if it succeeds 
     *
     * N.B. can't be prefix operator - according to Scala syntax only '+', '-', '~' and '!' can be prefix. 
     */
    def & = for (s <- get; a <- this; _ <- set(s)) yield a
    
    def -(exclude : => Rule[Any]) = !exclude -~ this
      
    def *~-(end : => Rule[Any]) = (this - end *) ~- end
    def +~-(end : => Rule[Any]) = (this - end +) ~- end

    /** ^~^(f) is equivalent to ^^ { case b1 ~ b2 => f(b1, b2) } 
     */
    def ^~^[B1, B2, B >: A <% B1 ~ B2, C](f : (B1, B2) => C) = map { a =>
      (a : B1 ~ B2) match { case b1 ~ b2 => f(b1, b2) } 
    }
    
     /** ^~~^(f) is equivalent to ^^ { case b1 ~ b2 ~ b3 => f(b1, b2, b3) } 
      */
    def ^~~^[B1, B2, B3, B >: A <% B1 ~ B2 ~ B3, C](f : (B1, B2, B3) => C) = map { a =>
      (a : B1 ~ B2 ~ B3) match { case b1 ~ b2 ~ b3 => f(b1, b2, b3) } 
    }
    
    /** ^~~~^(f) is equivalent to ^^ { case b1 ~ b2 ~ b3 ~ b4 => f(b1, b2, b3, b4) } 
     */
       def ^~~~^[B1, B2, B3, B4, B >: A <% B1 ~ B2 ~ B3 ~ B4, C](f : (B1, B2, B3, B4) => C) = map { a =>
       (a : B1 ~ B2 ~ B3 ~ B4) match { case b1 ~ b2 ~ b3 ~ b4 => f(b1, b2, b3, b4) } 
     }
     
    /** ^~~~~^(f) is equivalent to ^^ { case b1 ~ b2 ~ b3 ~ b4 ~ b5 => f(b1, b2, b3, b4, b5) } 
     */
    def ^~~~~^[B1, B2, B3, B4, B5, B >: A <% B1 ~ B2 ~ B3 ~ B4 ~ B5, C](f : (B1, B2, B3, B4, B5) => C) = map { a =>
      (a : B1 ~ B2 ~ B3 ~ B4 ~ B5) match { case b1 ~ b2 ~ b3 ~ b4 ~ b5 => f(b1, b2, b3, b4, b5) } 
    }
      
    /** ^~~~~~^(f) is equivalent to ^^ { case b1 ~ b2 ~ b3 ~ b4 ~ b5 ~ b6 => f(b1, b2, b3, b4, b5, b6) } 
     */
    def ^~~~~~^[B1, B2, B3, B4, B5, B6, B >: A <% B1 ~ B2 ~ B3 ~ B4 ~ B5 ~ B6, C](f : (B1, B2, B3, B4, B5, B6) => C) = map { a =>
      (a : B1 ~ B2 ~ B3 ~ B4 ~ B5 ~ B6) match { case b1 ~ b2 ~ b3 ~ b4 ~ b5 ~ b6 => f(b1, b2, b3, b4, b5, b6) } 
    }
       
     /** ^~~~~~~^(f) is equivalent to ^^ { case b1 ~ b2 ~ b3 ~ b4 ~ b5 ~ b6 => f(b1, b2, b3, b4, b5, b6) } 
      */
     def ^~~~~~~^[B1, B2, B3, B4, B5, B6, B7, B >: A <% B1 ~ B2 ~ B3 ~ B4 ~ B5 ~ B6 ~ B7, C](f : (B1, B2, B3, B4, B5, B6, B7) => C) = map { a =>
       (a : B1 ~ B2 ~ B3 ~ B4 ~ B5 ~ B6 ~ B7) match { case b1 ~ b2 ~ b3 ~ b4 ~ b5 ~ b6 ~b7 => f(b1, b2, b3, b4, b5, b6, b7) } 
     }
        
    /** Creates a rule that always succeeds with a Boolean value.  
     *  Value is 'true' if this rule succeeds, 'false' otherwise */
    def -? : Rule[Boolean] = map { any => true } | unit(false)
        
    /** >~>(f) is equivalent to >> { case b1 ~ b2 => f(b1, b2) } 
     */
    def >~>[B1, B2, B >: A <% B1 ~ B2, C](f : (B1, B2) => Rule[C]) = flatMap { a =>
      (a : B1 ~ B2) match { case b1 ~ b2 => f(b1, b2) } 
    }
  }
  
  object ZeroRule extends Rule[Nothing] with ZeroOrElse {
    def apply(s : S) = Failure
  }
  
  def rule[A](f : S => Result[(A, S)]) : Rule[A] = new Rule[A] { 
    def apply(s : S) = f(s)
    
    def flatMap[B](f : A => Rule[B]) = rule { 
      s : S => apply(s) match { 
        case Success(a, s) => f(a)(s) 
        case _ => Failure
      }
    }
    
    def orElse[B >: A](other : => Rule[B]) = rule { 
      s : S => apply(s).orElse(other(s))
    }
  }
  
  val failure = zero
  
}  
