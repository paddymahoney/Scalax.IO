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
import java.nio.charset._
import java.util.regex._
import scalax.control._

/** Adds extra methods to File. */
class FileExtras(file : File) {
	/** Deletes the file or directory recursively. Returns false if it failed. */
	def deleteRecursively() = FileHelp.deleteRecursively(file)

	/** Obtains a BufferedReader using the system default charset. */
	def reader =
		new UntranslatedManagedResource[BufferedReader] {
			def unsafeOpen() =
				new BufferedReader(new InputStreamReader(new FileInputStream(file)))
			def unsafeClose(r : BufferedReader) =
				r.close()
		}

	/** Obtains a BufferedReader using the supplied charset. */
	def reader(charset : String) = {
		// Do this lookup before opening the file, since it might fail.
		val cs = Charset.forName(charset)
		new UntranslatedManagedResource[BufferedReader] {
			def unsafeOpen() =
				new BufferedReader(new InputStreamReader(new FileInputStream(file), cs))
			def unsafeClose(r : BufferedReader) =
				r.close()
		}
	}

	/** Obtains a PrintWriter using the system default charset. */
	def writer =
		new UntranslatedManagedResource[PrintWriter] {
			def unsafeOpen() =
				new PrintWriter(file)
			def unsafeClose(r : PrintWriter) =
				r.close()
		}

	/** Obtains a PrintWriter using the supplied charset. */
	def writer(charset : String) =
		new UntranslatedManagedResource[PrintWriter] {
			def unsafeOpen() =
				new PrintWriter(file, charset)
			def unsafeClose(r : PrintWriter) =
				r.close()
		}

	/** Obtains a BufferedInputStream. */
	def inputStream =
		new UntranslatedManagedResource[BufferedInputStream] {
			def unsafeOpen() =
				new BufferedInputStream(new FileInputStream(file))
			def unsafeClose(s : BufferedInputStream) =
				s.close()
		}

	/** Obtains a BufferedOutputStream. */
	def outputStream =
		new UntranslatedManagedResource[BufferedOutputStream] {
			def unsafeOpen() =
				new BufferedOutputStream(new FileOutputStream(file))
			def unsafeClose(s : BufferedOutputStream) =
				s.close()
		}

	/** Attempts to return the file extension. */
	def extension = FileHelp.extension(file)

	/** Slurps the entire input file into a string, using the system default
	 * character set. */
	def slurp = for(r <- reader) yield StreamHelp.slurp(r)

	/** Slurps the entire input file into a string, using the supplied
	 * character set. */
	def slurp(charset : String) = for(r <- reader(charset)) yield StreamHelp.slurp(r)

	/** Writes the supplied string to the file, replacing any existing content,
	 * using the system default character set. */
	def write(s : String) = for(w <- writer) w.write(s)

	/** Writes the supplied string to the file, replacing any existing content,
	 * using the supplied character set. */
	def write(s : String, charset : String) = for(w <- writer(charset)) w.write(s)

	/** Copies the file. */
	def copyTo(dest : File) = FileHelp.copy(file, dest)

	/** Moves the file, by rename if possible, otherwise by copy-and-delete. */
	def moveTo(dest : File) = FileHelp.move(file, dest)

	/** Unzips the file into the specified directory. */
	def unzipTo(outdir : File) = FileHelp.unzip(file, outdir)

	def /(child : String) = new File(file, child)
}

/** Some shortcuts for file operations. */
object FileHelp {
	/** Deletes a file or directory recursively. Returns false if it failed. */
	def deleteRecursively(file : File) : Boolean = {
		if(file.isDirectory()) {
			for(i <- file.listFiles())
				deleteRecursively(i)
		}
		file.delete()
	}

	/** Unzips a file into the specified directory. */
	def unzip(zipfile : File, outdir : File) : Unit =
		StreamHelp.unzip(new FileInputStream(zipfile), outdir)

	private val extensionPattern = Pattern.compile(".*\\.([A-Za-z0-9_+-]+)$")
	/** Attempts to return the file extension. */
	def extension(file : File) : Option[String] = {
		val n = file.getName()
		val m = extensionPattern.matcher(n)
		if(m.matches()) Some(m.group(1)) else None
	}

	/** Copies a file. */
	def copy(src : File, dest : File) : Unit = {
		val in = new FileInputStream(src).getChannel
		try {
			val out = new FileOutputStream(dest).getChannel
			try {
				in.transferTo(0, in.size, out)
			} finally {
				out.close()
			}
		} finally {
			in.close()
		}
	}

	/** Moves a file, by rename if possible, otherwise by copy-and-delete. */
	def move(src : File, dest : File) : Unit = {
		dest.delete()
		if(!src.renameTo(dest)) {
			copy(src, dest)
			if(!src.delete()) throw new IOException("Delete after copy failed: "+src)
		}
	}
}
