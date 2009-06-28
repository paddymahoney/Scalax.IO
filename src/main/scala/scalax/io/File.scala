package scalax.io

import java.nio.charset.Charset
import collection.Traversable
import util.matching.Regex
import _root_.scalax.resource.ManagedResource


/** stuff that should be in an object somewhere.... */
trait Stuff {
  def createTempFile(prefix: String, suffix: String): File
  def userHome: Directory
  def currentWorkingDirectory: Directory
  def roots: Traversable[Location]
}


/**
 * TODO - Update this to work
 */
class GlobMapper(pattern : String) {
   def matches(path : String) : Boolean = false
}

/**
 * A point on a file system, such as a file, directory, or abstract pathname
 */
trait Location {
  type Location_T <: Location
  def canRead: Boolean
  def canWrite: Boolean
  def exists: Boolean
  def name: String
  def isAbsolute: Boolean
  def isHidden: Boolean
  /**
   * the number of seconds since the Epoch when this <code>Location</code was modified
   * @todo what should be returned if the file does not exist?
   */
  def lastModified: Long

  /**
   * true if this <code>Location</code> represents a directory
   * Note the small "d" in "directory," this location may be a <code>Path</code>
   * which can represent either a file or directory on the filesystem, but is neither
   * a <code>File</code> or <code>Directory</code> object.
   */
  def isDirectory: Boolean
  /** true if this <code>Location</code> represents a file */
  def isFile: Boolean
  /** the directory containing this <code>Location</code> */
  def parent: Option[Directory]
  def path: String
  def cannonicalPath: String
  def delete(): Boolean
  def renameTo(name: Location_T): Boolean
  /**
   * @return path to this <code>Location</code> relative to the specified <code>Directory</code>
   */
  def pathFrom(dir: Directory): String = {
    if (cannonicalPath.startsWith(dir.cannonicalPath)) {
      path.drop(dir.cannonicalPath.length)
    } else cannonicalPath
  }
}

trait Directory extends Location { self =>
  type Location_T <: Directory
  /** the full contents of this directory tree */
  def tree: Traversable[Location]
  /** the subdirectories immediately under this directory */
  def children: Traversable[Directory]
  /** the <code>File</code>s within this <code>Directory</code>*/
  def files: Traversable[File]
  def contents: Traversable[Location]
  /**
   * atomically create new empty Location with this pathname if one does not exist
   * @return true if a file was created, false if not
   */
  def create: Boolean
  final def isDirectory = true
  final def isFile = false

  def /(path: String): Path
  def /(glob: GlobMapper) = new Traversable[Location] {
     def foreach[U](f: Location => U) = self.tree.filter(x => glob.matches(x.pathFrom(self))).foreach(f)
  }
  def /(regex: Regex) = new Traversable[Location] {
     def foreach[U](f: Location => U) = self.tree.filter( x => regex.pattern.matcher(x.pathFrom(self)).matches).foreach(f)
  }
}

//TODO - Complete this interface!!!
trait File extends Location { self =>
  type Location_T <: File
  final def isDirectory = false
  final def isFile = true
  /**
   * the length of the this <code>File</code> in bytes
   * @todo what should this return if the file does not exist?  java.io.File will return 0L,
   *       the same as for an empty file.
   */
  def length: Long
  /**
   * atomically create new empty Location with this pathname if one does not exist
   * @return true if a file was created, false if not
   */
  def create(): Boolean
  def inputStream: InputStream
  def outputStream: OutputStream
//  def copyTo(dest: File): Unit
//  def moveTo(dest: Location): Unit
//TODO: make charset a constant somewhere instead of using forName repeated
  def reader(implicit charset : Charset = Charset.forName("UTF-8")) = inputStream.reader(charset)
  def writer(implicit charset : Charset = Charset.forName("UTF-8")) = outputStream.writer(charset)

  def lines(implicit charset : Charset = Charset.forName("UTF-8")) : Traversable[String] = ManagedStreams.lines(reader(charset).buffered)(LineEndingStyle.ALL) //TODO - Figure out implicit/defaults issue!!!
  def chars(implicit charset : Charset = Charset.forName("UTF-8")) : Traversable[Char] = ManagedStreams.chars(reader(charset).buffered)
  def bytes : Traversable[Byte] = ManagedStreams.bytes(inputStream.buffered)
  def slurp : Array[Byte] = for(in <- ManagedResource(inputStream)) yield in.slurp

  def writeLines(lines : Iterable[String])(implicit charset : Charset = Charset.forName("UTF-8")) : Unit = for(out <- ManagedResource(writer)) {
        out.writeLines(lines)
  }
  def write(input : String)(implicit charset : Charset = Charset.forName("UTF-8")) : Unit = for(out <- ManagedResource(writer)) {
        out.writeChars(input)
  }
  def write(input : Array[Byte]) : Unit = for(out <- ManagedResource(outputStream)) {
        out.write(input)()
  }
}


trait Path extends Location {
  type Location_T <: Path
  /**
   * Convert this <code>Path</code> into a <code>Directory</code<
   * @return a <code>Directory</code> object representing this path
   * @throws IllegalStateException if this path is already a file
   */
  def asDirectory: Directory
  /**
   * Convert this <code>Path</code> into a <code>File</code>
   * @return a <code>File</code> object representing this path
   * @throws IllegalStateException if this path already exists as a directory
   */
  def asFile: File
  /**
   * @returns <code>None</code> if the path doesn't exist, or an appropriate typed object if it does
   */
  def asFileOrDirectory: Option[Either[File, Directory]]
  /**
   * Creates a new file if this <code>Path</code> does not exist
   * @return <code>Some(file)</code> if a file was created, otherwise <code>None</code>
   */
  def createNewFile(): Option[File]
  /**
   * Creates a new directory if this <code>Path</code> does not exist
   * @return <code>Some(directory)</code> if a directory was created, otherwise <code>None</code>
   */
  def createNewDirectory(): Option[Directory]
}





