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

package scalax.io
import java.io._
import java.text._
import scala.collection.mutable._
import scalax.data._

/** An iterator interface to a CSV data stream: generates an array of strings
 * representing each row. Note that the underlying Reader should be closed
 * manually (or by a ManagedResource) if iteration is not completed. */
class CsvIterator(csv : Reader) extends Iterator[Array[String]] {
	/** The separator character, a comma by default, but could be overridden to
	 * any character which is not whitespace or '"'. */
	def sep = ','

	private val br = new BufferedReader(csv)
	private var nextLine = br.readLine()
	if(nextLine == null) br.close()
	private var lineNo = 0

	def hasNext : Boolean = (nextLine != null)
	def next : Array[String] = {
		var fields = new ArrayBuffer[String]
		var chars = nextLine.toCharArray()
		var len = chars.length
		lineNo = lineNo + 1
		nextLine = br.readLine()
		if(nextLine == null) br.close()
		var i = -1
		while(i < len) {
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
						if(nextLine == null)
							throw new ParseException(lineNo+":"+(i + 1)+
									": Mismatched quotes", lineNo)
						chars = nextLine.toCharArray()
						len = chars.length
						lineNo += 1
						nextLine = br.readLine()
						if(nextLine == null) br.close()
						i = 0
					} else if(i + 1 < len && chars(i + 1) == '"') {
						field.append('"')
						i = i + 2
					} else {
						done = true
					}
				}
				i += 1
				while(i < len && chars(i) != sep) {
					if(!Character.isWhitespace(chars(i)))
						throw new ParseException(lineNo+":"+(i + 1)+
								": Garbage after close quote", lineNo)
					i += 1
				}
				fields += field.toString()
			} else {
				// Non-quoted field
				val start = i
				while(i < len && chars(i) != sep) i += 1
				var end = i - 1
				while(end >= start && Character.isWhitespace(chars(end))) end = end - 1
				fields += new String(chars, start, end - start + 1)
			}
		}
		fields.toArray
	}
}
