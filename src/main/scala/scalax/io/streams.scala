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

import scala.io.Codec
/** This enumeration represents various line ending styles that could be used for ReaderStreams */
object LineEndingStyle extends Enumeration {
   
   type LineEndingStyle = Value
   
   val WINDOWS, UNIX, MAC, ALL = Value
   
   /**
    * Returns the line separator characters for a given type of line separate
    */
   def separator_for(style : Value) = style match {
       case WINDOWS => "\r\n"
       case UNIX    => "\n"
       case MAC     => "\r"
       case _       => "\n"  //Default to unix
   } 
   /**
    * Attempts to determine which style of line separate a given set of characters is. 
    */
   def constant_from_separator(sep : String) = sep match {
       case "\r\n" => WINDOWS
       case "\r"   => MAC
       case "\n"   => UNIX
       case _ => ALL //Default to any of them
   }
   /**
    * Saved version of the platform line separator.
    */
   private val platform_line_separator = System.getProperty("line.separator", "\n")

   /** Returns the current platform line separator style. */ 
   def current_platform_style = constant_from_separator(platform_line_separator)
}

/**
 * This trait provides methods available on all the synchronous io streams.
 */
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
   def reader(implicit codec: Codec = Codec.default) : ReaderStream
   /** Blocking call to write the contents of this stream to an output stream */
   def pumpTo(output : OutputStream) : Unit
   /** Blocking call to write the contents of this stream to an output stream */
   def >>>(output : OutputStream) = pumpTo(output)
}
/**
 * This represents an input stream over Character/Textual data.   This is used to pull in lines/characters from some source.
 */
trait ReaderStream extends IOStream[ReaderStream] {
   /** Lazily created sequence of lines in this input stream.  This will refetch lines every time it's iterated over */
   def lines(implicit lineEnding : LineEndingStyle.LineEndingStyle = LineEndingStyle.current_platform_style) : Iterable[String]
   /** Lazily created sequence of characters in this input stream.  This will refetch characters every time it's iterated over */
   def chars : Iterable[Char]
   /** Eagerly loads the entire stream into memory */
   def slurp : Array[Char]
   /** Blocking call to write the contents of this ReaderStream to the WriterStream. */
   def pumpTo(output : WriterStream) : Unit
   /** Blocking call to write the contents of this ReaderStream to the WriterStream. */
   def >>>(output : WriterStream) = pumpTo(output)
}

/**
 * This represents an input stream that can pull in serialized objects 
 */
trait ObjectInputStream extends IOStream[ObjectInputStream] {
   /** Returns an iterable over all the serialized objects in this inputStream. */
   def objects : Iterable[Any]
   /** Blocking call to read all serialized objects in this input stream and return them in memory */
   def slurp : Array[Any]
   /** Blocking call to serialize the objects from the ObjectInputStream to this stream */                     
   def pumpTo(output : ObjectOutputStream) : Unit
   /** Blocking call to serialize the objects from the ObjectInputStream to this stream */
   def >>>(output : ObjectOutputStream) = pumpTo(output)
}
/**
 * This represents an output stream that writes serialized objects 
 */
trait ObjectOutputStream extends IOStream[ObjectOutputStream] {
   /** Serializes an Object to this output stream */
   def writeObject[A](input : A) : Unit
   /** Serializes all of a collection of objects to this output stream */
   def writeAll[A](input : Iterable[A]) : Unit
   /** Pumps all objects from a given input stream into this output stream */
   def pumpFrom(input : ObjectInputStream) : Unit
   /** Pumps all objects from a given input stream into this output stream */
   def <<<(input : ObjectInputStream) = pumpFrom(input)
}
/**
 * This represents an output stream that writes pure binary data.
 */
trait OutputStream extends IOStream[OutputStream] {
   /**  Write the sequence of bytes into this output stream*/
   def write(input: Iterable[Byte]) : Unit
   /** Writes a single byte to this stream */
   def write(input: Byte) : Unit
   /** Writes an number of bytes defined by length, found in input to this stream starting at a given offset */
   def write(input: Array[Byte])(length: Int = input.length, offset: Int = 0) : Unit
   /** Returns a reader for this InputStream */
   def writer(implicit codec: Codec = Codec.default) : WriterStream
   /** Blocking call to write the contents of the input stream to this stream */
   def pumpFrom(input: InputStream) : Unit = input >>> this
   /** Blocking call to write the contents of the input stream to this stream */
   def <<<(input: InputStream) = pumpFrom(input)
   /** Returns a buffered version of this stream */
   def buffered : OutputStream
}

/**
 * This represents an output stream that writes character/textual data.
 */
trait WriterStream extends IOStream[WriterStream] {
  val lineEndingStyle: LineEndingStyle.LineEndingStyle
  //TODO: implement writeLine in terms of write(String)
  def writeLine(input: String): Unit = {
	write(input)
	write(LineEndingStyle.separator_for(lineEndingStyle))
  }
  /** Writes the sequence of string to the stream assuming each string is a line in the file */
  //TODO: implement writeLines in terms of writeLine
  def writeLines(input: Iterable[String]) = input.foreach(writeLine)
  /**
   * Writes the sequence of characters to the stream in the stream's encoding.
   * @param input the characters to write
   */
  def writeChars(input: Iterable[Char]): Unit = input.foreach(write(_))
  /**
   * write the specified <code>String</code> using the stream's encoding
   * @param input the string to write
   */
  def write(input: String): Unit
  /** Writes a character to this stream */
  def write(input: Char) : Unit   
  /** Blocking call to write the contents of the input stream to this stream */
  def pumpFrom(input: ReaderStream) : Unit = input >>> this
  /** Blocking call to write the contents of the input stream to this stream */
  def <<<(input: ReaderStream) = pumpFrom(input)
}
