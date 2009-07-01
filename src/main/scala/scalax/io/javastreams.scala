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
import _root_.java.nio.charset.Charset

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
	def lines(br : jio.BufferedReader): Iterator[String] = new FetchIterator[String] {
		override def fetchNext() =
			br.readLine() match {
				case null => None
				case s => Some(s)
			}
	}   

	/** Iterates over the lines of the reader. */
	def lines(in : jio.Reader) : Iterator[String] = {
		val br = ensureBuffered(in)
		lines(br)
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
   def bytes : Iterable[Byte] = new Iterable[Byte] {
       def iterator = JavaStreamHelp.bytes(s)
   }
   /** Eagirly fetched array of bytes from the entire input stream */
   def slurp : Array[Byte] = JavaStreamHelp.slurp(s)
   /** Returns a reader for this InputStream */
   def reader(implicit charset : Charset) : ReaderStream = new JavaReaderStreamWrapper(new jio.InputStreamReader(s, charset))
   /** Blocking call to write the contents of this stream to an output file */
   def pumpTo(output : OutputStream) : Unit = {
       //TODO - Speed this up!
       for ( byte <- bytes ) { 
           output.write(byte) 
       }
   }
   /** Closes this resource */
   def close() : Unit = { s.close(); }
	
}

class JavaReaderStreamWrapper(s : jio.Reader) extends ReaderStream {
   /** Lazily created sequence of bytes in this input stream.  This will refetch bytes every time it's iterated over */
   def slurp : Array[Char] = null
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
   def lines(implicit lineEnding : LineEndingStyle.LineEndingStyle = LineEndingStyle.current_platform_style) : Iterable[String] = new Iterable[String] {
      def iterator = JavaStreamHelp.lines(s)
   }
   /** Lazily created sequence of characters in this input stream.  This will refetch characters every time it's iterated over */
   def chars : Iterable[Char] = new Iterable[Char] {
      def iterator = JavaStreamHelp.chars(s)
   }
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
   def writer(implicit charset : Charset = Charset.forName("UTF-8")) : WriterStream = new JavaWriterStreamWrapper(new jio.OutputStreamWriter(s, charset))
}

object JavaFileOutputStreamWrapper {
  def apply(opt: WriteOption, file: jio.File) = {
    if (file.exists()) {
      if (!opt.openExisting) throw new FileAlreadyExists(file.getName())
    } else {
      if (!opt.createNew) throw new FileDoesNotExist(file.getName())
    }
    val s = new jio.FileOutputStream(file, opt.append)
    new JavaFileOutputStreamWrapper(opt, file, s)
  }
}

class JavaFileOutputStreamWrapper protected (val opt: WriteOption, protected val file: jio.File, override protected val s: jio.FileOutputStream) 
      extends JavaOutputStreamWrapper(s) {
  override def close(): Unit = {
    super.close()
    if (opt.deleteOnClose) {
      if (!file.delete()) throw new FileDeletionFailed(file.getName())
    }
  }
} 

class JavaWriterStreamWrapper(s : jio.Writer) extends WriterStream {
   /** Returns a buffered version of this stream */
   def buffered : WriterStream = new JavaWriterStreamWrapper(new jio.BufferedWriter(s)) {
         override def buffered = this
   }
   /** Closes this stream */
   def close() : Unit = s.close()
   /** Writes the sequence of string to the stream assuming each string is a line in the file */
   def writeLines(input : Iterable[String], lineEnding : LineEndingStyle.LineEndingStyle = LineEndingStyle.current_platform_style) : Unit = {
       input.foreach(s.write)
   }
   /** Writes the sequence of characters to the stream in the stream's encoding. */
   def writeChars(input : Iterable[Char]) : Unit = input.foreach(s.write(_))
   /** Writes a character to this stream */
   def write(input : Char) : Unit = s.write(input)
}

object JavaConversions {
   implicit def inputStream(s : jio.InputStream) = new JavaInputStreamWrapper(s)
   implicit def outputStream(s : jio.OutputStream) = new JavaOutputStreamWrapper(s)
   implicit def readerStream(s : jio.Reader) = new JavaReaderStreamWrapper(s)
   implicit def writerStream(s : jio.Writer) = new JavaWriterStreamWrapper(s)

}

