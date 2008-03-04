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
	
	/* Obtains a Reader using the system default charset. */
	def reader =
		new ReaderResource[Reader] {
			// XXX: should be UTF-8 by default instead of OS default
			// practically, here in Russia I never used default charset
			def unsafeOpen() =
				new InputStreamReader(InputStreamResource.this.unsafeOpen())
		}
	
	/** Obtains a Reader using the supplied charset. */
	def reader(charset : String) = {
		// Do this lookup before opening the file, since it might fail.
		val cs = Charset.forName(charset)
		new ReaderResource[Reader] {
			def unsafeOpen() =
				new InputStreamReader(InputStreamResource.this.unsafeOpen(), cs)
		}
	}
	
	def lines = reader.lines
	
	def lines(charset : String) = reader(charset).lines
}

abstract class ReaderResource[R <: Reader] extends CloseableResource[R] {
	def buffered : ReaderResource[BufferedReader] =
		new ReaderResource[BufferedReader] {
			def unsafeOpen() = new BufferedReader(ReaderResource.this.unsafeOpen())
			override def buffered = this
		}
	
	def lines =
		new ManagedSequence[String] {
			type Handle = Reader
			val resource = ReaderResource.this
			def iterator(v : Reader) = StreamHelp.lines(v)
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
			def unsafeOpen() =
				new BufferedOutputStream(OutputStreamResource.this.unsafeOpen())
			override def buffered = this
		}
	
	// XXX: should use UTF-8, see comment above
	/** Obtains a Writer using the system default charset. */
	def writer =
		new WriterResource[Writer] {
			def unsafeOpen() =
				new OutputStreamWriter(OutputStreamResource.this.unsafeOpen())
		}
	
	/** Obtains a Writer using the supplied charset. */
	def writer(charset : String) = {
		val cs = Charset.forName(charset)
		new WriterResource[Writer] {
			def unsafeOpen() =
				new OutputStreamWriter(OutputStreamResource.this.unsafeOpen(), cs)
		}
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
}

// vim: set ts=4 sw=4 noet:
