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
import java.util.zip._
import java.net.URL
import java.nio.channels._
import java.nio.charset._
import java.util.regex._
import scalax.control.{ManagedResource, ManagedSequence}

abstract class ResourceFactory {
	/** Resource type */
	type RT
	
	def file(f : File) : RT
	
	def file(path : String) : RT = file(new File(path))
	
	def url(u : String) : RT
	
	def apply(path : String) = {
		if (path.startsWith("/") || path.startsWith("./")) file(new File(path))
		else url(path)
	}
}

abstract class CloseableResource[C <: Closeable] extends ManagedResource[C] { self =>
	type Handle = C
	final def translate(v : Handle) = v
	override def unsafeClose(r : Handle) = r.close()
	
	protected trait Wrapper {
		type Handle
		type SelfHandle = self.Handle
		
		def wrap(is : SelfHandle) : Handle
		
		final def unsafeOpen() = {
			val is = self.unsafeOpen()
			try {
				wrap(is)
			} catch {
				case e =>
					self.unsafeCloseQuietly(is)
					throw e
			}
		}
	}
}

abstract class InputStreamResource extends CloseableResource[InputStream] {
	def buffered : InputStreamResource =
		new InputStreamResource with Wrapper {
			override def wrap(is : SelfHandle) = new BufferedInputStream(is)
			
			override def buffered = this
		}

	def slurp() = for (is <- this) yield StreamHelp.slurp(is)
	
	/* Obtains a Reader using the system default charset. */
	def reader =
		new ReaderResource with Wrapper {
			// XXX: should be UTF-8 by default instead of OS default
			// practically, here in Russia I never used default charset
			override def wrap(is : SelfHandle) = new InputStreamReader(is)
		}
	
	def gunzip =
		new InputStreamResource with Wrapper {
			override def wrap(is : SelfHandle) = new GZIPInputStream(is)
		}
	
	/** Obtains a Reader using the supplied charset. */
	def reader(charset : String) = {
		// Do this lookup before opening the file, since it might fail.
		val cs = Charset.forName(charset)
		new ReaderResource with Wrapper {
			override def wrap(is : SelfHandle) = new InputStreamReader(is, cs)
		}
	}
	
	def lines = reader.lines
	
	def lines(charset : String) = reader(charset).lines
	
	def readLines() = reader.readLines()
	
	def readLine() = reader.readLine()
	
	def pumpTo(osr : OutputStreamResource) {
		// Note InputStream should be opened before OutputStream
		for (is <- this; os <- osr) StreamHelp.pump(is, os)
	}
}

object InputStreamResource extends ResourceFactory {
	override type RT = InputStreamResource
	
	def apply(is : => InputStream) : InputStreamResource =
		new InputStreamResource {
			override def unsafeOpen() = is
		}
	
	def bytes(array : Array[Byte], offset : Int, length : Int) =
		apply(new ByteArrayInputStream(array, offset, length))
	
	def bytes(b : Array[Byte]) : InputStreamResource =
		bytes(b, 0, b.length)
	
	override def file(file : File) =
		apply(new FileInputStream(file))

	private def url(url : java.net.URL) = {
		def is = {
			// see org.springframework.core.io.UrlResource
			val conn = url.openConnection()
			conn.setUseCaches(false)
			conn.getInputStream()
		}
		apply(is)
	}
	
	def classpath(path : String): InputStreamResource =
		classpath(path, Thread.currentThread.getContextClassLoader)
	
	def classpath(path : String, classLoader: ClassLoader): InputStreamResource = {
		def is = {
			val is = classLoader.getResourceAsStream(path)
			if (is eq null) throw new FileNotFoundException("classpath resource " + path + " not found")
			is
		}
		apply(is)
	}

	private val CLASSPATH_URL_PREFIX = "classpath:"
	
	private val GZIP_URL_PREFIXES = List("gzip:", "gunzip:")
	
	override def url(u : String): InputStreamResource = {
		if (u startsWith CLASSPATH_URL_PREFIX) classpath(u.substring(CLASSPATH_URL_PREFIX.length))
		else {
			val gzippedO = GZIP_URL_PREFIXES.map(prefix => (prefix, u startsWith prefix))
					.find(_._2)
					.map(_._1)
			if (gzippedO.isDefined) {
				url(u.substring(gzippedO.get.length)).gunzip
			} else url(new URL(u))
		}
	}
	
}

abstract class ReaderResource extends CloseableResource[Reader] {

	def slurp() = for (r <- this) yield StreamHelp.slurp(r)
	
	def buffered : ReaderResource =
		new ReaderResource with Wrapper {
			override def wrap(r : SelfHandle) = new BufferedReader(r)
			
			override def buffered = this
		}
	
	def lines =
		new ManagedSequence[String] {
			type Handle = Reader
			override val resource = ReaderResource.this
			override def iterator(v : Reader) = StreamHelp.lines(v)
		}
	
	def readLines(): Seq[String] = lines.toList
	
	/** First line or <code>""</code> if file is empty */
	def readLine() = lines.firstOption.getOrElse("")
	
	def pumpTo(wr : WriterResource) {
		// Note Reader should be opened before Writer
		for (r <- this; w <- wr) StreamHelp.pump(r, w)
	}
}

object ReaderResource extends ResourceFactory  {
	override type RT = ReaderResource

	def string(s : String) =
		apply(new StringReader(s))
	
	def apply(r : => Reader) =
		new ReaderResource {
			override def unsafeOpen() = r
		}
	
	override def file(f : File) =
		InputStreamResource.file(f).reader
	
	override def url(u : String) =
		InputStreamResource.url(u).reader
	
	def classpath(p : String) =
		InputStreamResource.classpath(p).reader
}

abstract class OutputStreamResource extends CloseableResource[OutputStream] {
	
	def buffered : OutputStreamResource =
		new OutputStreamResource with Wrapper {
			override def wrap(os : SelfHandle) = new BufferedOutputStream(os)
			
			override def buffered = this
		}
	
	// XXX: should use UTF-8, see comment above
	/** Obtains a Writer using the system default charset. */
	def writer =
		new WriterResource with Wrapper {
			override def wrap(os : SelfHandle) = new OutputStreamWriter(os)
		}
	
	def gzip =
		new OutputStreamResource with Wrapper {
			override def wrap(os : SelfHandle) = new GZIPOutputStream(os)
		}
	
	/** Obtains a Writer using the supplied charset. */
	def writer(charset : String) = {
		val cs = Charset.forName(charset)
		new WriterResource with Wrapper {
			override def wrap(os : SelfHandle) = new OutputStreamWriter(os, cs)
		}
	}
	
	def writeLine(line : String) = writer.writeLine(line)
	
	def writeLines(lines : Seq[String]) = writer.writeLines(lines)
	
	def writeString(string : String) = writer.writeString(string)
	
	def pumpFrom(isr : InputStreamResource) {
		isr pumpTo this
	}
}

object OutputStreamResource extends ResourceFactory {
	override type RT = OutputStreamResource
	
	def apply[O <: OutputStream](os : => O) =
		new OutputStreamResource {
			def unsafeOpen() = os
		}

	def fileAppend(file : File, append : Boolean) =
		apply(new FileOutputStream(file, append))
	
	def fileAppend(file : File) : OutputStreamResource =
		fileAppend(file, true)
		
	override def file(file : File) =
		apply(new FileOutputStream(file))
		
	private val FILE_URL_PREFIX = "file:"
	private val GZIP_URL_PREFIX = "gzip:"
	
	override def url(u : String) : OutputStreamResource = {
		if (u startsWith FILE_URL_PREFIX) file(new File(u substring FILE_URL_PREFIX.length))
		else if (u startsWith GZIP_URL_PREFIX) url(u substring GZIP_URL_PREFIX.length).gzip
		else throw new IllegalArgumentException("unknown url: " + u)
	}
	
}

abstract class WriterResource extends CloseableResource[Writer] {
	
	def buffered : WriterResource =
		new WriterResource with Wrapper {
			override def wrap(w : SelfHandle) = new BufferedWriter(w)
			
			override def buffered = this
		}
	
	def writeString(string : String) {
		for (w <- this) w.write(string)
	}
	
	/** Alias for <code>writeString</code> */
	def write(string : String) = writeString(string)
	
	/** Write strings adding line separator after each line */
	def writeLines(lines : Seq[String]) {
		for (w <- buffered; line <- lines) {
			w.write(line)
			w.write(FileHelp.lineSeparator)
		}
	}
	
	/** Write string followed by line separator */
	def writeLine(line : String) {
		writeLines(line :: Nil)
	}
	
	def pumpFrom(rr : ReaderResource) {
		rr pumpTo this
	}
}

object WriterResource extends ResourceFactory {
	override type RT = WriterResource
	
	def apply(w : => Writer) =
		new WriterResource {
			override def unsafeOpen() = w
		}
	
	override def file(f : File) =
		OutputStreamResource.file(f).writer
	
	override def url(u : String) =
		OutputStreamResource.url(u).writer
}

// vim: set ts=4 sw=4 noet:
