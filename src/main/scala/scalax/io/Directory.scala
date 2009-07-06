
package scalax.io


import collection.Traversable
import util.matching.Regex

trait DirectoryFactory {
  /**
   * Create a new <code>Directory<code> object with the specified pathname
   * This does not create a new directory on the file system.  Use the Directory.create method to create a
   * new directory on the file system.
   * @param pathName a relative, absolute, or canonical pathname
   * @return a new <code>Directory</code> object
   * @throws IllegalArgumentException if the specified <code>pathName</code> is an existing <code>File</code>
   */
  def apply(pathName: String): Directory
  /**
   * Create a new directory within the specified parent directory
   * This does not create a new directory on the file system.
   * @param parent the parent <code>Directory</code> for the new <code>Directory<code>
   * @param name the pathname relative to <code>parent</code>
   * @return a new <code>Directory</code> within <code>parent</code>
   */
  def apply(parent: Directory, name: String): Directory
  /**
   * the directory for storing temporary files
   */
  def temp: Directory
  /**
   * the home directory of the current user
   */ 
  def home: Directory
  /**
   * the current working directory
   */ 
  def current: Directory
  /**
   * root directories
   * On Unix-like systems this returns a single directory, namely "/".  On Windows this returns a list
   * of directories representing the drive letters of currently mapped drives.
   */
  def roots: Traversable[Directory]
}

object Directory extends DirectoryFactory {
  //TODO: make the implementation of DirectoryFactory use by Directory configurable
  val impl: DirectoryFactory = JavaDirectory
  def apply(name: String) = impl(name)
  def apply(dir: Directory, name: String) = impl(dir, name)
  def temp = impl.temp
  def home = impl.home
  def current = impl.current
  def roots = impl.roots
}


trait Directory extends Location with DirectoryOpsMixin { self =>
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
  /** @return true */
  final def isDirectory = true
  /** @return false */
  final def isFile = false
  protected def asDirectory = self
  /**
   * Create a new file object within this directory (does not actually add the file to filesystem)
   * @param name the name of the new <code>File</code>
   * @return a new <code>File</code> object for a file within this directory
   */
  def newFile(name: String): File
  /**
   * Create a new subdirectory within this directory (does not actually add the directory to the filesystem)
   * @param name the name of the new <code>Directory</code>
   * @return a new <code>Directory</code> object for a directory within this <code>Directory</code>
   */
  def newDirectory(name: String): Directory
  /**
   * Create a new path within this directory (does not actually add path to the filesystem)
   * @param name the name of the new <code>Path</code>
   * @return a new <code>Path</code> object for a path within this <code>Directory</code<
   */ 
  def newPath(name: String): Path
}

private[io] trait DirectoryOpsMixin extends Location { self =>
  protected def asDirectory: Directory
  def /(path: String): Path  // this needs to be a Path instead of a Location so that several levels
			     // can be specified together
  def /(glob: GlobMapper) = new Traversable[Location] {
    def foreach[U](f: Location => U) =
      self.tree.filter(x => glob.matches(x.pathFrom(self.asDirectory))).foreach(f)
  }
  def /(regex: Regex) = new Traversable[Location] {
    def foreach[U](f: Location => U) =
      self.tree.filter( x => regex.pattern.matcher(x.pathFrom(self.asDirectory)).matches).foreach(f)
  }
  /** the full contents of this directory tree */
  def tree: Traversable[Location]
  //TODO: def deleteRecursively from scala.io.File
}

import java.{ io => jio }

object JavaDirectory extends DirectoryFactory {
  def apply(name: String) = apply(new jio.File(name))
  def apply(file: jio.File): JavaDirectory = {
    if (file.isFile()) throw new IllegalArgumentException("The specified location is an existing file: " + file.getName())
    new JavaDirectory(file)
  }
  def apply(dir: Directory, name: String): JavaDirectory = JavaDirectory(JavaDirectory(dir), name)
  def apply(dir: Directory): JavaDirectory = dir match {
    case jdir: JavaDirectory => jdir
    case _ => JavaDirectory(dir.name)
  }
  def temp = {
    val tmpDirName = System.getProperty("java.io.tmpdir")
    JavaDirectory(tmpDirName)
  }
  def home = JavaDirectory(System.getProperty("user.home"))
  def current = JavaDirectory(System.getProperty("user.dir"))
  def roots: Traversable[JavaDirectory] = jio.File.listRoots.map(JavaDirectory(_))
}

private[io] trait JavaDirectoryMixin extends DirectoryOpsMixin with JavaLocation {
  def /(path : String) = new JavaPath(new jio.File(file, path))
  def tree: Traversable[JavaPath] = new Traversable[JavaPath] {
    def foreach[U](f: JavaPath => U) = (new JFileTree(file)).foreach(jf => f(new JavaPath(jf)))
  }
  def contents: Traversable[JavaPath] = file.listFiles.map(new JavaPath(_))
}

final class JavaDirectory(protected[io] val file: jio.File) extends Directory with JavaDirectoryMixin {
  def create = file.mkdirs()
  def children: Traversable[JavaDirectory] = file.listFiles.filter(_.isDirectory).map(new JavaDirectory(_))
  def files: Traversable[JavaFile] = file.listFiles.filter(_.isFile).map(JavaFile(_))
  def newFile(name: String): JavaFile = JavaFile(this, name)
  def newDirectory(name: String): JavaDirectory = JavaDirectory(this, name)
  def newPath(name: String): JavaPath = JavaPath(this, name)
}

// Note: calling list() on a file (as in not a directory) returns null
//       calling list() on an empty directory returns an empty array

import scala.collection.mutable.Queue
/** Represents the preorder traversal of the directory tree rooted at the given file. */
class JFileTree(val root : jio.File) extends Iterable[jio.File] {
  def iterator = new Walk
 
  class Walk private[JFileTree] extends Iterator[jio.File] {
    private val files = new Queue[jio.File]();
    private val dirs = new Queue[jio.File]();
    private var failed = false;
    dirs.enqueue(root)
 
    private def enqueue(dir: jio.File) = {
      val list = dir.listFiles()
      if(list == null) {
        failed = true;
      } else {
        for (file <- dir.listFiles()) {
          if (file.isDirectory()) dirs.enqueue(file)
          else files.enqueue(file)
        }
      }
      dir
    }
 
    def hasNext = files.length != 0 || dirs.length != 0
 
    def next() = {
      failed = false;
      if (!hasNext) throw new NoSuchElementException("No more results")
      if (files.length != 0) files.dequeue else enqueue(dirs.dequeue)
    }
 
    def wasUnreadable = failed;
  }
}

// This is just an idea for providing a parent directory for windows drive letters so that
// other code doesn't have to deal with the fact that there are multiple root directories.
// I'm going to leave it for now because I'm working on a Mac and haven't setup a Windows VM.
//class SyntheticRootDirectory(private val roots: Traversable[Directory]) extends Directory {
//  def children = roots
//  def files: Traversable[File] = Nil
//  def contents: Traversable[Location] = roots
//  def create: Boolean = throw new UnsupportedOperationException()
//  
//}

