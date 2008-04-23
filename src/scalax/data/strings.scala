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
import java.io._
import java.math._
import java.net._

/** Provides extra methods for Strings. */
class StringExtras(s : String) {
	def urlEncode = URLEncoder.encode(s, "UTF-8")
	def urlDecode = URLDecoder.decode(s, "UTF-8")
	def xmlEscape = StringHelp.xmlEscape(s)
	def cEscape = StringHelp.cEscape(s)
	def uncapitalize = StringHelp.uncapitalize(s)
	def toBoolean = StringHelp.toBoolean(s)
	def toFile = new File(s)
	def toURL = new URL(s)
	def pad(l : Int, c : Char) : String = StringHelp.pad(s, l, c)
	def pad(l : Int) : String = pad(l, ' ')

	def toOptInt =
		try {
			Some(Integer.parseInt(s))
		} catch {
			case e : NumberFormatException => None
		}

	def toOptDouble =
		try {
			Some(java.lang.Double.parseDouble(s))
		} catch {
			case e : NumberFormatException => None
		}

	def toBigDecimal = new BigDecimal(s)

	def toOptBigDecimal =
		try {
			Some(toBigDecimal)
		} catch {
			case e : NumberFormatException => None
		}

	def truncate(maxLen : Int, terminator : String) =
		StringHelp.truncate(s, maxLen, terminator)

	def reader = new BufferedReader(new StringReader(s))
}

/** Some convenience functions for string manipulation. */
object StringHelp {
	/** XML-escapes a string. */
	def xmlEscape(x : String) =
		(x
			.replace("&", "&amp;")
			.replace("\"", "&quot;")
			.replace("<", "&lt;")
		)

	/** Backslash-escapes a string */
	def cEscape(x : String) =
		(x
			.replace("\\", "\\\\")
			.replace("\n", "\\n")
			.replace("\'", "\\\'")
			.replace("\"", "\\\"")
			.replace("\r", "\\r")
			.replace("\t", "\\t")
		)

	/** Truncates a string, and adds a terminator afterwards. Attempts to stop
	 * at whitespace if there's some within 10 characters of the maximum
	 * length. */
	def truncate(s : String, maxLen : Int, terminator : String) =
		if(s.length <= maxLen) {
			s
		} else if(maxLen < 11) {
			s.substring(0, maxLen) + terminator
		} else {
			val i1 = s.lastIndexOf(' ', maxLen)
			val i2 = s.lastIndexOf('\n', maxLen)
			val i = if(i1 > i2) i1 else i2
			if(i == -1 || i < maxLen - 10)
				s.substring(0, maxLen) + terminator
			else
				s.substring(0, i) + terminator
		}

	/** Uncapitalizes the first letter of a string */
	def uncapitalize(s : String) =
		if(s.length == 0) s
		else s.substring(0,1).toLowerCase() + s.substring(1)

	/** Gets the truth value of a string. */
	def toBoolean(s : String) : Boolean =
		s.toLowerCase match {
			case "true" | "yes" | "1" | "on" | "t" | "y" => true
			case "false" | "no" | "0" | "off" | "f" | "n" => false
			case _ => throw new NumberFormatException("Unknown truth value: "+s)
		}

	/** Pads the string up to the given length using the given character. */
	def pad(s : String, l : Int, c : Char) : String =
		if(s.length >= l) {
			s
		} else {
			val b = new StringBuilder(s)
			var i = s.length
			while(i < l) {
				i += 1
				b += c
			}
			b.toString
		}
}
