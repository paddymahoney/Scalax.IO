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

package scalax.data
import java.math.{BigDecimal => JBigDecimal}

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

object Numbers {
	/** Returns an approximate value of n, to the nearest 1/divs of 1
	 * significant figure. */
	def approximate(n : Long, divs : Int) : Long = {
		val log = java.lang.Math.log10(n).toInt
		val scale = Math.pow(10, log).toLong
		(n * divs * 10L / scale + 5L) / 10L * scale / divs
	}

	private val si = Array(
		"y", "z", "a", "f", "p", "n", "µ", "m", "",
		"k", "M", "G", "T", "P", "E", "Z", "Y"
	)

	/** Returns n as a String, with trailing zeros replaced by an SI
	 * abbreviation. */
	def asSi(n : JBigDecimal, suffix : String) : String = {
		if(n.compareTo(JBigDecimal.ZERO) == 0) {
			"0" + suffix
		} else {
			val stripped = n.stripTrailingZeros
			val scale = stripped.scale
			val scale3 = if(scale >= 0) (scale + 2) / 3 else scale / 3
			val rscale = if(scale3 > 8) 8 else if(scale3 < -8) -8 else scale3
			val scaled = stripped.movePointRight(rscale * 3)
			val prefix = si(-rscale + 8)
			scaled.toPlainString + prefix + suffix
		}
	}

	def asSi(n : BigDecimal, suffix : String) : String =
		asSi(n.bigDecimal, suffix)
	def asSi(n : Long, scale : Int, suffix : String) : String =
		asSi(new JBigDecimal(n).movePointLeft(scale), suffix)
	def asSi(n : Long) : String =
		asSi(new JBigDecimal(n), "")
}
