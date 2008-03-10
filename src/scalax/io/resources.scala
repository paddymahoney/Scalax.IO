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
	
	def url(u : String): InputStreamResource[InputStream] = {
		if (u startsWith CLASSPATH_URL_PREFIX) classpath(u.substring(CLASSPATH_URL_PREFIX.length))
		else url(new URL(u))
	}
		
}

abstract class ReaderResource[R <: Reader] extends CloseableResource[R] {
	def slurp() = for (r <- this) yield StreamHelp.slurp(r)
	
	def buffered : ReaderResource[BufferedReader] =
		new ReaderResource[BufferedReader] {
			def unsafeOpen() = new BufferedReader(ReaderResource.this.unsafeOpen())
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
			def unsafeOpen() =
				new BufferedOutputStream(OutputStreamResource.this.unsafeOpen())
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
}

object OutputStreamResource {
	def file(file : File) =
		new OutputStreamResource[FileOutputStream] {
			def unsafeOpen() =
				new FileOutputStream(file)
		}
}

abstract class WriterResource[W <: Writer] extends CloseableResource[W] {
	def buffered : WriterResource[BufferedWriter] =
		new WriterResource[BufferedWriter] {
			def unsafeOpen() =
				new BufferedWriter(WriterResource.this.unsafeOpen())
			override def buffered = this
		}
	
	def printWriter =
		new WriterResource[PrintWriter] {
			def unsafeOpen() =
				new PrintWriter(WriterResource.this.unsafeOpen())
		}
	
	def writeString(string : String) {
		for (w <- this) w.write(string)
	}
	
	def writeLines(lines : Seq[String]) {
		for (w <- buffered; line <- lines) {
			w.write(line)
			w.write(FileHelp.lineSeparator)
		}
	}
	
	def writeLine(line : String) {
		writeLines(line :: Nil)
	}
}

// vim: set ts=4 sw=4 noet:
