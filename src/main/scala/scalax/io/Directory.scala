
package scalax.io


import collection.Traversable
import util.matching.Regex

trait DirectoryFactory {
  def apply(name: String): Directory
  def tempDirectory: Directory
  def userHomeDirectory: Directory
  def currentWorkingDirectory: Directory
}

object Directory extends DirectoryFactory {
  val impl: DirectoryFactory = JavaDirectory
  def apply(name: String): Directory = impl(name)
  def tempDirectory: Directory = impl.tempDirectory
  def userHomeDirectory: Directory = impl.userHomeDirectory
  def currentWorkingDirectory: Directory = impl.currentWorkingDirectory
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
  final def isDirectory = true
  final def isFile = false
  protected def asDirectory = self
}

private[io] trait DirectoryOpsMixin extends Location { self =>
  protected def asDirectory: Directory
  def /(path: String): Path  // this needs to be a Path instead of a Location because so that several levels
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
  def apply(dir: Directory): JavaDirectory = dir match {
    case jdir: JavaDirectory => jdir
    case _ => JavaDirectory(dir.name)
  }
  def tempDirectory = {
    val tmpDirName = System.getProperty("java.io.tmpdir")
    JavaDirectory(tmpDirName)
  }
  def userHomeDirectory = JavaDirectory(System.getProperty("user.home"))
  def currentWorkingDirectory = JavaDirectory(System.getProperty("user.dir"))
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
