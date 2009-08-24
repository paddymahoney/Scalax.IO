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

import _root_.java.{io => jio}
import _root_.scala.io.Codec
import _root_.scala.annotation.tailrec
/**
 * This is a basic abstraction for an iterator that fetches new content as needed.
 */
private[io] abstract class FetchIterator[T] extends Iterator[T] {

	var fetched = false
	var nextItem : Option[T] = None
	
	protected def fetchNext(): Option[T]
	
	override def hasNext = {
		if (!fetched) {
			nextItem = fetchNext()
			fetched = true
		}
		nextItem.isDefined
	}
	
	override def next() = {
		if (!hasNext) throw new NoSuchElementException("EOF")
		fetched = false
		nextItem.get
	}
	
}
/**
 * This class is used to iterate over lines in a reader using *strict* line ending characters
 */
private[io] class LineFetchIterator(br : jio.BufferedReader, val lineEndingString :String) extends FetchIterator[String] {
  protected def fetchNext() : Option[String] = {
      val output = new StringBuilder
      
      
      @tailrec
      def readInput(input : Int, endingIndex : Int) : Unit = {
          input match {
               case -1 => //Return!

               case x if endingIndex > 0 && (x.toChar != lineEndingString(endingIndex)) => 
                   //Add buffered "endline check chars" to buffer
                   output.appendAll(lineEndingString.take(endingIndex))
                   //Append most recently read character
                   output.append(x.toChar)
                   readInput(br.read(), 0)

               case x if (x.toChar == lineEndingString(endingIndex)) && (endingIndex+1 == lineEndingString.length) => 
                    //Return

               case x if endingIndex > 0 => //The line ending characters have to match because of earlier case statement!
                   readInput(br.read(), endingIndex + 1)

               case x if (x.toChar == lineEndingString(0)) =>
                   readInput(br.read(), endingIndex + 1)

               case x => 
                   output.append(x.toChar)
                   readInput(br.read(), endingIndex)
          }
      }
      readInput(br.read(), 0)
      val finalOutput = output.toString
      if(finalOutput.length > 0) Some(finalOutput) else None 
  }
}

private[io] object JavaStreamHelp {
        /** Pumps all data from the input stream through to the output stream.
	 * Returns the number of bytes transferred. */
	def pump(in : jio.InputStream, out : jio.OutputStream) : Int = {
		val buf = new Array[Byte](65536)
		var len = in.read(buf)
		var count = 0
		while(len > -1) {
			out.write(buf, 0, len)
			count = count + len
			len = in.read(buf)
		}
		count
	}
	/** Slurps the entire input stream into a byte array. */
	def slurp(in : jio.InputStream) : Array[Byte] = {
		val out = new jio.ByteArrayOutputStream
		pump(in, out)
		out.toByteArray()
	}
        /** Lazily iterates over bytes in the inputstream */
	def bytes(in : jio.InputStream) : Iterator[Byte] = new FetchIterator[Byte] {
               override def fetchNext() = {
                  val input = in.read()
                  if(input != -1) Some(input.toByte) else None
               }
	}

       /** Lazily reads characters from the stream */
       def chars(in : jio.Reader) : Iterator[Char] = new FetchIterator[Char] {
               override def fetchNext() = {
                  val input = in.read()
                  if(input != -1) Some(input.toChar) else None
               }
         
       }

     /** 
      * Iterates over the lines of the reader.
      * Keeps reader open even after reaching EOF, reader must be closed explicitly.
      */
	def lines(br : jio.BufferedReader, lineEndingStyle : LineEndingStyle.LineEndingStyle): Iterator[String] = lineEndingStyle match {
             case LineEndingStyle.ALL => new FetchIterator[String] {
                         override def fetchNext() = br.readLine() match {
                           case null => None
                           case s => Some(s)
                         }
                       }
            case _ => new LineFetchIterator(br, LineEndingStyle.separator_for(lineEndingStyle))
        }   

	/** Iterates over the lines of the reader. */
	def lines(in : jio.Reader, lineEndingStyle : LineEndingStyle.LineEndingStyle) : Iterator[String] = {
		val br = ensureBuffered(in)
		lines(br, lineEndingStyle)
	}  

	/** Wrap this Reader into a BufferedReader if it isn't one already. */
	def ensureBuffered(r : jio.Reader) : jio.BufferedReader = r match {
			case b : jio.BufferedReader => b
			case _ => new jio.BufferedReader(r)
		}
}

class JavaInputStreamWrapper(protected val s: jio.InputStream) extends InputStream {
   /** Returns a buffered version of this input stream */
   def buffered : InputStream = new JavaInputStreamWrapper(new jio.BufferedInputStream(s)) {
       override def buffered = this
   }
   /** Lazily created sequence of bytes in this input stream.  This will refetch bytes every time it's iterated over */
   def bytes : Iterable[Byte] = JavaStreamHelp.bytes(s)
   
   /** Eagirly fetched array of bytes from the entire input stream */
   def slurp : Array[Byte] = JavaStreamHelp.slurp(s)
   /** Returns a reader for this InputStream */
   def reader(implicit codec: Codec): ReaderStream = new JavaReaderStreamWrapper(new jio.InputStreamReader(s, codec.decoder))
   /** Blocking call to write the contents of this stream to an output file */
   def pumpTo(output: OutputStream) : Unit = {
       //TODO - Speed this up!
       for ( byte <- bytes ) { 
           output.write(byte) 
       }
   }
   /** Closes this resource */
   def close() : Unit = { s.close(); }
	
}

class JavaObjectInputStreamWrapper(protected val s : jio.ObjectInputStream) extends ObjectInputStream {
   /** Returns a buffered version of this stream */
   def buffered : ObjectInputStream = this
   /** Closes this stream */
   def close() : Unit = s.close()
   
   /** Returns an iterable over all the serialized objects in this inputStream. */
   def objects : Iterator[Any] =  new FetchIterator[Any] {
      override def fetchNext() = {
          val input = s.readObject()
          if(input != null) Some(input) else None
     }
   }
   
   /** Blocking call to read all serialized objects in this input stream and return them in memory */
   def slurp : Array[Any] = {
       val buffer = new scala.collection.mutable.ArrayBuffer[Any]
       val iterator = objects.iterator
       while(iterator.hasNext) {
          buffer += iterator.next
       }
       return buffer.toArray
   }
   /** Blocking call to serialize the objects from the ObjectInputStream to this stream */                     
   def pumpTo(output : ObjectOutputStream) : Unit = {
       for(obj <- objects) {
          output.writeObject(obj)
       }
   }
}

class JavaReaderStreamWrapper(s : jio.Reader) extends ReaderStream {
   /** Lazily created sequence of bytes in this input stream.  This will refetch bytes every time it's iterated over */
   def slurp : Array[Char] = {
      val buffer = new scala.collection.mutable.ArrayBuffer[Char]
      val iterator = chars.iterator
      while(iterator.hasNext) {
         buffer += iterator.next
      }
      return buffer.toArray
   }
   /** Blocking call to write the contents of this stream to an output file */
   def pumpTo(output : WriterStream) : Unit = {
       for(char <- chars) {
           output.write(char)
       }
   }
   /** Closes this resource */
   def close() : Unit = { s.close(); }
   /** Returns a buffered version of this input stream */
   def buffered : ReaderStream = this
   /** Lazily created sequence of lines in this input stream.  This will refetch lines every time it's iterated over */
   def lines(implicit lineEnding : LineEndingStyle.LineEndingStyle = LineEndingStyle.current_platform_style) : Iterable[String] = JavaStreamHelp.lines(s, lineEnding)
   
   /** Lazily created sequence of characters in this input stream.  This will refetch characters every time it's iterated over */
   def chars : Iterable[Char] = JavaStreamHelp.chars(s)
}

class JavaOutputStreamWrapper(protected val s: jio.OutputStream) extends OutputStream {
   /** Returns a buffered version of this stream */
   def buffered : OutputStream = new JavaOutputStreamWrapper(new jio.BufferedOutputStream(s)) {
       override def buffered = this
   }
   /** Closes this stream */
   def close() : Unit = s.close()
   
  /**  Write the sequence of bytes into this output stream*/
   def write(input : Iterable[Byte]) : Unit = {
      for(b <- input) {
          s.write(b)
      }
   }
   /** Writes a single byte to this stream */
   def write(input : Byte) : Unit = s.write(input)
   /** Writes an number of bytes defined by length, found in input to this stream starting at a given offset */
   def write(input : Array[Byte])(length : Int = input.length, offset : Int = 0) : Unit = s.write(input, offset,length)
   /** Returns a reader for this InputStream */
   def writer(implicit codec: Codec = Codec.default) : WriterStream = new JavaWriterStreamWrapper(new jio.OutputStreamWriter(s, codec.encoder))
}

class JavaObjectOutputStreamWrapper(protected val s : jio.ObjectOutputStream) extends ObjectOutputStream {
   def buffered = this
   /** Closes this stream */
   def close() : Unit = s.close()
   
   
   /** Serializes an Object to this output stream */
   def writeObject[A](input : A) : Unit = s.writeObject(input)

   /** Serializes all of a collection of objects to this output stream */
   def writeAll[A](input : Iterable[A]) : Unit = {
        for(b <- input) {
           writeObject(b)
       }
    } 
   /** Pumps all objects from a given input stream into this output stream */
   def pumpFrom(input : ObjectInputStream) : Unit = input.pumpTo(this)


}

class JavaFileOutputStreamWrapper(val opt: WriteOption, protected val file: File, override protected val s: jio.FileOutputStream) 
      extends JavaOutputStreamWrapper(s) {
  override def close(): Unit = {
    super.close()
    if (opt.deleteOnClose) file.delete()
  }
} 

class JavaWriterStreamWrapper(protected val s: jio.Writer, 
		                      val lineEndingStyle: LineEndingStyle.LineEndingStyle = LineEndingStyle.current_platform_style)
	  extends WriterStream {
  /** Returns a buffered version of this stream */
  def buffered: WriterStream = s match {
	case bw: jio.BufferedWriter => this
	case _ => new JavaWriterStreamWrapper(new jio.BufferedWriter(s)) {
      override def buffered = this
    }
  }
  /** Closes this stream */
  def close(): Unit = s.close()
  /** Writes a character to this stream */
  def write(input: Char): Unit = s.write(input)
  def write(input: String): Unit = s.write(input)
}




object JavaConversions {
   implicit def inputStream(s : jio.InputStream) = new JavaInputStreamWrapper(s)
   implicit def outputStream(s : jio.OutputStream) = new JavaOutputStreamWrapper(s)
   implicit def readerStream(s : jio.Reader) = new JavaReaderStreamWrapper(s)
   implicit def writerStream(s : jio.Writer) = new JavaWriterStreamWrapper(s)
   implicit def objectInputStream(s : jio.ObjectInputStream) = new JavaObjectInputStreamWrapper(s)
   implicit def objectOutputStream(s : jio.ObjectOutputStream) = new JavaObjectOutputStreamWrapper(s)
}

