package scalax.data

import collection._

object misc {

  /**
   * @deprecated Use {@link Iterator.from} instead.
   */
  @serializable
  class Uniq(var x: Int) {
    def next = {
      x = x + 1
      x - 1
    }
  }

  // TODO: Better name?
  /**
   * Funnel a value into the range [0,max].
   */
  def bounds(max: Int, x: Int): Int = bounds(0, max, x)

  /**
   * Funnel a value into the range [min,max].
   */
  def bounds(min: Int, max: Int, x: Int) = Math.max(min, Math.min(max-1, x))

  // TODO: Better name?
  /**
   * Return the positive modulus x mod m.
   */
  def wrapMod(x: Int, m: Int) = if (x < 0) m + x % m else x % m

  def succ(x: Int) = x + 1
  def pred(x: Int) = x - 1
  def const[a](f: => a): Any => a = ((x: Any) => f)

  // TODO: move this to the same place as optionize and success in Control.
  /**
   * Return the result of f on success or null if there was an exception.
   */
  def nullifyExceptions[a >: Null](f: => a): a = try { f } catch { case e => null }

  /**
   * "Simplify" a String by trimming and lower-casing it.
   */
  def simplify(s: String) = s.trim.toLowerCase

  /**
   * Rich string view.
   */
  case class XString(val s: String) {
    /**
     * Case-insensitive comparator.
     */
    def ===(that: String) = s equalsIgnoreCase that
    /**
     * Case-insensitive comparator.
     */
    def !==(that: String) = !(===(that))
    /**
     * Repeat the string n times.
     */
    def *(n: Int) = repeat(s) take n mkString ("", "", "")
    /**
     * Truncate a string to be of length n.
     */
    def truncate(n: Int) = s substring (0, s.length - n)
    /**
     * String interpolator.
     */
    def %[A](s: A*): String =
      format(s.map(_.asInstanceOf[AnyRef]).toArray)
    /**
     * String interpolator.
     */
    def format(a: Array[AnyRef]) = String.format(s, a)
  }
  implicit def str2xstr(s: String): XString = XString(s)
  implicit def xstr2str(s: XString): String = s.s

  /**
   * Identity function.
   */
  def id[a](x: a) = x

  /**
   * Generate Strings that are the given base string with a unique integer
   * appended.
   */
  class HandleGenerator(base: String) extends Iterator[String] {
    val i = Iterator from 0
    override def next = base + i.next
    override def hasNext = i.hasNext
  }
}
