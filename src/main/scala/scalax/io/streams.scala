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

import _root_.java.nio.charset.Charset


object LineEndingStyle extends Enumeration {
   type LineEndingStyle = Value
    val windows,unix,mac = Value

    def current_platform_style = unix // TODO - Fix
}

trait IOStream[T] {
   /** Returns a buffered version of this stream */
   def buffered : T
   /** Closes this stream */
   def close() : Unit
}


/**
 * This represents an input stream, used to pull in binary data from some source.
 */
trait InputStream extends IOStream[InputStream] {
   /** Lazily created sequence of bytes in this input stream.  This will refetch bytes every time it's iterated over */
   def bytes : Iterable[Byte]
   /** Eagerly loaded array of bytes from the rest of this input stream */
   def slurp : Array[Byte]
   /** Returns a reader for this InputStream */
   def reader(implicit charset : Charset = Charset.forName("UTF-8")) : ReaderStream
   /** Blocking call to write the contents of this stream to an output stream */
   def pumpTo(output : OutputStream) : Unit
   def >>>(output : OutputStream) = pumpTo(output)
   def buffered : InputStream
}

trait ReaderStream extends IOStream[ReaderStream] {
   /** Lazily created sequence of lines in this input stream.  This will refetch lines every time it's iterated over */
   def lines(implicit lineEnding : LineEndingStyle.LineEndingStyle = LineEndingStyle.current_platform_style) : Iterable[String]
   /** Lazily created sequence of characters in this input stream.  This will refetch characters every time it's iterated over */
   def chars : Iterable[Char]
   /** Eagerly loads the entire stream into memory */
   def slurp : Array[Char]
   /** Blocking call to write the contents of this stream to an output stream. */
   def pumpTo(output : WriterStream) : Unit
   def >>>(output : WriterStream) = pumpTo(output)
   def buffered : ReaderStream
}

trait OutputStream extends IOStream[OutputStream] {
   /**  Write the sequence of bytes into this output stream*/
   def write(input : Iterable[Byte]) : Unit
   /** Writes a single byte to this stream */
   def write(input : Byte) : Unit
   /** Writes an number of bytes defined by length, found in input to this stream starting at a given offset */
   def write(input : Array[Byte])(length : Int = input.length, offset : Int = 0) : Unit
   /** Returns a reader for this InputStream */
   def writer(implicit charset : Charset = Charset.forName("UTF-8")) : WriterStream
   /** Blocking call to write the contents of the input stream to this stream */
   def pumpFrom(input : InputStream) : Unit = input >>> this
   def <<<(input : InputStream) = pumpFrom(input)
   def buffered : OutputStream
}


trait WriterStream extends IOStream[WriterStream] {
   /** Writes the sequence of string to the stream assuming each string is a line in the file */
   def writeLines(input : Iterable[String], lineEnding : LineEndingStyle.LineEndingStyle = LineEndingStyle.current_platform_style) : Unit
   /** Writes the sequence of characters to the stream in the stream's encoding. */
   def writeChars(input : Iterable[Char]) : Unit
   /** Writes a character to this stream */
   def write(input : Char) : Unit   
   /** Blocking call to write the contents of the input stream to this stream */
   def pumpFrom(input : ReaderStream) : Unit = input >>> this
   def <<<(input : ReaderStream) = pumpFrom(input)
   def buffered : WriterStream
}
