// -----------------------------------------------------------------------------
//
//  Scalax - The Scala Community Library
//  Copyright (c) 2005-7 The Scalax Project. All rights reserved.
//
//  The primary distribution site is http://scalax.scalaforge.org/
//
//  This software is released under the terms of the Revised BSD License.
//  There is NO WARRANTY.  See the file LICENSE for the full text.
//
// -----------------------------------------------------------------------------

package scalax.data

/** Converts strings to integers as part of a pattern match, e.g.:
 * <pre>
 *     "42" match {
 *         case AsInt(x) => println(x + 5)
 *     }
 * </pre>
 */
object AsInt {
	def unapply(s : String) : Option[Int] =
		try {
			Some(Integer.parseInt(s))
		} catch {
			case e : NumberFormatException => None
		}
}

class IntExtras(i : Int) {
	def seconds = i * 1000L
	def minutes = 60 * seconds
	def hours = 60 * minutes
	def days = 24 * hours
	def weeks = 7 * days

	/** Returns the number of milliseconds in this number of months, where a
	 * month is taken to be one twelfth of an average year. */
	def months = years / 12

	/** Returns the number of milliseconds in this number if years, where a year
	 * is taken to be 31556952 seconds, which is the average length of a year
	 * in the Gregorian calendar. */
	def years = i * 31556952000L
}
