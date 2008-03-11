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
import scalax.control._

abstract class CloseableResource[C <: Closeable] extends UntranslatedManagedResource[C] {
    override def unsafeClose(r : C) = r.close()
}

abstract class InputStreamResource[I <: InputStream] extends CloseableResource[I] {
	def buffered : InputStreamResource[BufferedInputStream] =
		new InputStreamResource[BufferedInputStream] {
			def unsafeOpen() = new BufferedInputStream(InputStreamResource.this.unsafeOpen())
			override def buffered = this
		}

	def slurp() = for (is <- this) yield StreamHelp.slurp(is)
	
	/* Obtains a Reader using the system default charset. */
	def reader =
		new ReaderResource[Reader] {
			// XXX: should be UTF-8 by default instead of OS default
			// practically, here in Russia I never used default charset
			def unsafeOpen() = {
				val is = InputStreamResource.this.unsafeOpen()
				try {
					new InputStreamReader(is)
				} catch {
					case e =>
						InputStreamResource.this.unsafeCloseQuietly(is)
						throw e
				}
			}
		}
	
	/** Obtains a Reader using the supplied charset. */
	def reader(charset : String) = {
		// Do this lookup before opening the file, since it might fail.
		val cs = Charset.forName(charset)
		new ReaderResource[Reader] {
			def unsafeOpen() = {
				val is = InputStreamResource.this.unsafeOpen()
				try {
					new InputStreamReader(is, cs)
				} catch {
					case e =>
						InputStreamResource.this.unsafeCloseQuietly(is)
						throw e
				}
			}
		}
	}
	
	def lines = reader.lines
	
	def lines(charset : String) = reader(charset).lines
	
	def readLines() = reader.readLines()
	
	def readLine() = reader.readLine()
	
	def pumpTo[O <: OutputStream](osr : OutputStreamResource[O]) {
		// Note InputStream should be opened before OutputStream
		for (is <- this; os <- osr) StreamHelp.pump(is, os)
	}
}

object InputStreamResource {
	def bytes(array : Array[Byte], offset : Int, length : Int) =
		new InputStreamResource[ByteArrayInputStream] {
			def unsafeOpen() = new ByteArrayInputStream(array, offset, length)
		}
	
	def bytes(b : Array[Byte]) : InputStreamResource[ByteArrayInputStream] =
		bytes(b, 0, b.length)
	
	def file(file : File) =
		new InputStreamResource[FileInputStream] {
			def unsafeOpen() = new FileInputStream(file)
		}

	def file(path : String): InputStreamResource[FileInputStream] = file(new File(path))

	private def url(url : java.net.URL) =
		new InputStreamResource[InputStream] {
			def unsafeOpen() = {
				// see org.springframework.core.io.UrlResource
				val conn = url.openConnection()
				conn.setUseCaches(false)
				conn.getInputStream()
			}
		}
	
	def classpath(path : String): InputStreamResource[InputStream] =
		classpath(path, Thread.currentThread.getContextClassLoader)
	
	def classpath(path : String, classLoader: ClassLoader): InputStreamResource[InputStream] =
		new InputStreamResource[InputStream] {
			def unsafeOpen() = {
				val is = classLoader.getResourceAsStream(path)
				if (is eq null) throw new FileNotFoundException
				is
			}
		}
	
	private val CLASSPATH_URL_PREFIX = "classpath:"
	
	private val GZIP_URL_PREFIXES = List("gzip:", "gunzip:")
	
	def url(u : String): InputStreamResource[InputStream] = {
		if (u startsWith CLASSPATH_URL_PREFIX) classpath(u.substring(CLASSPATH_URL_PREFIX.length))
		else {
			val gzippedO = GZIP_URL_PREFIXES.map(prefix => (prefix, u startsWith prefix))
					.find(_._2)
					.map(_._1)
			if (gzippedO.isDefined) {
				// XXX: avoid cast
				gunzip(url(u.substring(gzippedO.get.length))).asInstanceOf[InputStreamResource[InputStream]]
			} else url(new URL(u))
		}
	}
	
	def gunzip[I <: InputStream](iss : InputStreamResource[I]) =
		new InputStreamResource[GZIPInputStream] {
			override def unsafeOpen() = {
				val is = iss.unsafeOpen()
				try {
					new GZIPInputStream(is)
				} catch {
					case e =>
						iss.unsafeClose(is)
						throw e
				}
			}
		}
}

abstract class ReaderResource[R <: Reader] extends CloseableResource[R] {
	def slurp() = for (r <- this) yield StreamHelp.slurp(r)
	
	def buffered : ReaderResource[BufferedReader] =
		new ReaderResource[BufferedReader] {
			def unsafeOpen() = {
				val reader = ReaderResource.this.unsafeOpen()
				try {
					new BufferedReader(reader)
				} catch {
					case e =>
						ReaderResource.this.unsafeCloseQuietly(reader)
						throw e
				}
			}
			
			override def buffered = this
		}
	
	def lines =
		new ManagedSequence[String] {
			type Handle = BufferedReader
			val resource = ReaderResource.this.buffered
			override def iterator(v : BufferedReader) = StreamHelp.lines(v)
		}
	
	def readLines(): Seq[String] = lines.toList
	
	/** First line or <code>""</code> if file is empty */
	def readLine() = lines.headOption.getOrElse("")
	
	def pumpTo[W <: Writer](wr : WriterResource[W]) {
		// Note Reader should be opened before Writer
		for (r <- this; w <- wr) StreamHelp.pump(r, w)
	}
}

object ReaderResource {
	def string(s : String) =
		new ReaderResource[StringReader] {
			def unsafeOpen() = new StringReader(s)
		}
}

abstract class OutputStreamResource[O <: OutputStream] extends CloseableResource[O] {
	def buffered : OutputStreamResource[BufferedOutputStream] =
		new OutputStreamResource[BufferedOutputStream] {
			def unsafeOpen() = {
				val os = OutputStreamResource.this.unsafeOpen()
				try {
					new BufferedOutputStream(os)
				} catch {
					case e =>
						OutputStreamResource.this.unsafeCloseQuietly(os)
						throw e
				}
			}
			
			override def buffered = this
		}
	
	// XXX: should use UTF-8, see comment above
	/** Obtains a Writer using the system default charset. */
	def writer =
		new WriterResource[Writer] {
			def unsafeOpen() = {
				val os = OutputStreamResource.this.unsafeOpen()
				try {
					new OutputStreamWriter(os)
				} catch {
					case e =>
						OutputStreamResource.this.unsafeCloseQuietly(os)
						throw e
				}
			}
		}
	
	/** Obtains a Writer using the supplied charset. */
	def writer(charset : String) = {
		val cs = Charset.forName(charset)
		new WriterResource[Writer] {
			def unsafeOpen() = {
				val os = OutputStreamResource.this.unsafeOpen()
				try {
					new OutputStreamWriter(os, cs)
				} catch {
					case e =>
						OutputStreamResource.this.unsafeCloseQuietly(os)
						throw e
				}
			}
		}
	}
	
	def writeLine(line : String) = writer.writeLine(line)
	
	def writeLines(lines : Seq[String]) = writer.writeLines(lines)
	
	def writeString(string : String) = writer.writeString(string)
	
	def pumpFrom[I <: InputStream](isr : InputStreamResource[I]) {
		isr pumpTo this
	}
}

object OutputStreamResource {
	def fileAppend(file : File, append : Boolean) =
		new OutputStreamResource[FileOutputStream] {
			def unsafeOpen() =
				new FileOutputStream(file, true)
		}
	
	def fileAppend(file : File) : OutputStreamResource[FileOutputStream] =
		fileAppend(file, true)
		
	def file(file : File) =
		new OutputStreamResource[FileOutputStream] {
			def unsafeOpen() =
				new FileOutputStream(file)
		}
}

abstract class WriterResource[W <: Writer] extends CloseableResource[W] {
	def buffered : WriterResource[BufferedWriter] =
		new WriterResource[BufferedWriter] {
			def unsafeOpen() = {
				val writer = WriterResource.this.unsafeOpen()
				try {
					new BufferedWriter(writer)
				} catch {
					case e =>
						WriterResource.this.unsafeCloseQuietly(writer)
						throw e
				}
			}
			
			override def buffered = this
		}
	
	def printWriter: WriterResource[PrintWriter] =
		new WriterResource[PrintWriter] {
			def unsafeOpen() = {
				val writer = WriterResource.this.unsafeOpen()
				try {
					new PrintWriter(writer)
				} catch {
					case e =>
						WriterResource.this.unsafeCloseQuietly(writer)
						throw e
				}
			}
			
			override def printWriter: WriterResource[PrintWriter] = this
		}
	
	def writeString(string : String) {
		for (w <- this) w.write(string)
	}
	
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
	
	def pumpFrom[R <: Reader](rr : ReaderResource[R]) {
		rr pumpTo this
	}
}

// vim: set ts=4 sw=4 noet:
