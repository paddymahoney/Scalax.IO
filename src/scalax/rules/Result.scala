package scalax.rules;
/** 
 * Throw this exception or a subclass to indicate a failure without alternatives
 */
class RuleException[Context](val context : Context, message : String) extends Exception(message)

object Result extends Monads {
  type M[+A] = Result[A]
  
  def unit[A](a : => A) = Success(a)
  override def zero = Failure
}
  
/** 
 * A Result is either Success or Failure.
 *
 * @author Andrew Foggin
 */
sealed abstract class Result[+A] extends Result.Monad[A] with Result.OrElse[A]
      
/** 
 * Result of a rule that was successfully applied.  
 */
case class Success[+A](value : A) extends Result[A] {
  def flatMap[B](f : A => Result[B]) = f(value)
  def orElse[B >: A](other : => Result[B]) = this
}
      
/** 
 * Result of a rule that could not be applied.
 */
case object Failure extends Result[Nothing] with Result.ZeroOrElse
