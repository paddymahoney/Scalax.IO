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

package scalax.io
import java.io._
import java.text._
import scala.collection.mutable._
import scalax.data._
import scalax.io.Implicits._

/** An iterator interface to a CSV data stream: generates an array of strings
 * representing each row. Note that the underlying Reader should be closed
 * manually (or by a ManagedResource) if iteration is not completed. */
class CsvIterator(csv : Reader) extends Iterator[Array[String]] {
	/** The separator character, a comma by default, but could be overridden to
	 * any character which is not whitespace or '"'. */
	def sep = ','

	/** The number of fields each row should have. Default is 0, which means
	 * any number. */
	def arity = 0

	/** If true, ignore blank lines and treat commentStart outside quotes as
	 * starting a comment line. Default is false. */
	def comments = false

	/** The comment start character. Default '#'. Only effective if comments is
	 * true. */
	def commentStart = '#'

	private val br = csv.ensureBuffered
	private var nextLine = ""
	private var lineNo = 0
	getNext()

	private def getNext() : Unit = {
		nextLine = br.readLine()
		lineNo += 1
		if(nextLine == null) {
			br.close()
		} else if(comments) {
			var i = 0
			val len = nextLine.length
			while(i < len && Character.isWhitespace(nextLine.charAt(i))) i += 1
			if(i == len || nextLine.charAt(i) == commentStart) getNext()
		}
	}

	def hasNext : Boolean = (nextLine != null)
	def next : Array[String] = {
		var fields = new ArrayBuffer[String]
		var chars = nextLine.toCharArray()
		var len = chars.length
		var i = -1
		while(i < len) {
			val dropComment = i == -1 || chars(i) != sep
			i += 1
			while(i < len && Character.isWhitespace(chars(i))) i += 1
			if(i < len && chars(i) == '"') {
				// Quoted field
				val field = new StringBuilder
				i += 1
				var done = false
				while(!done) {
					val start = i
					while(i < len && chars(i) != '"') i += 1
					field.append(chars, start, i - start)
					if(i == len) {
						field.append('\n')
						nextLine = br.readLine()
						if(nextLine == null) {
							br.close()
							throw new ParseException(lineNo+":"+(i + 1)+
									": Mismatched quotes", lineNo)
						}
						chars = nextLine.toCharArray()
						len = chars.length
						lineNo += 1
						i = 0
					} else if(i + 1 < len && chars(i + 1) == '"') {
						field.append('"')
						i = i + 2
					} else {
						done = true
					}
				}
				i += 1
				while(i < len && chars(i) != sep && !(comments && chars(i) == commentStart)) {
					if(!Character.isWhitespace(chars(i)))
						throw new ParseException(lineNo+":"+(i + 1)+
								": Garbage after close quote", lineNo)
					i += 1
				}
				if(i < len && comments && chars(i) == commentStart) i -= 1
				fields += field.toString()
			} else if(comments && i < len && chars(i) == commentStart) {
				// Comment
				if(!dropComment) fields += ""
				i = len
			} else {
				// Non-quoted field
				val start = i
				while(i < len && chars(i) != sep && !(comments && chars(i) == commentStart)) i += 1
				var end = i - 1
				if(i < len && comments && chars(i) == commentStart) i -= 1
				while(end >= start && Character.isWhitespace(chars(end))) end = end - 1
				fields += new String(chars, start, end - start + 1)
			}
		}
		if(arity != 0 && fields.length != arity)
			throw new ParseException(lineNo+":1: Found "+fields.length+
					" fields but was expecting "+arity, lineNo)
		getNext()
		fields.toArray
	}
}

class KeyValueIterator(r : Reader) extends Iterator[(String, String)] {
	private val csv = new CsvIterator(r) {
		override def sep = '='
		override def arity = 2
		override def comments = true
	}

	def hasNext = csv.hasNext
	def next = {
		val a = csv.next
		(a(0), a(1))
	}
}
