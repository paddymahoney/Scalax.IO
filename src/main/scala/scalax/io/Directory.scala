
package scalax.io


import collection.Traversable
import util.matching.Regex
import java.{ io => jio }

private[io] trait DirectoryOps[T <: DirectoryOps[T]] extends Location { 
  self: T =>
  protected def toDirectory: Directory
  def /(name: String): Path = Path(self.toDirectory, name)
  def /(glob: GlobMapper) = new Traversable[Path] {
    def foreach[U](f: Path => U) =
      self.tree.filter(x => glob.matches(x.pathFrom(self.toDirectory))).foreach(f)
  }
  def /(regex: Regex) = new Traversable[Path] {
    def foreach[U](f: Path => U) =
      self.tree.filter( x => regex.pattern.matcher(x.pathFrom(self.toDirectory)).matches).foreach(f)
  }
  def tree: Traversable[Path] = new Traversable[Path] {
     def foreach[U](f: Path => U) = (new JFileTree(jfile)).foreach(jf => f(new Path(jf)))
  }
  def contents: Traversable[Path] = handleSecurity {
    val files = jfile.listFiles()
    if (files eq null) {
      // File.listFiles returns null if the directory does not exist or if it is a regular file
      if (jfile.exists) {
    	// the path exists on the file system, so it must be a file and not a directory
        throw new PathIsAFile(self)
      } else {
    	// the path doesn't exist, so it can't have any contents
    	throw new LocationDoesNotExist(self)
      }
    }
    files.map(new Path(_))
  }
  def children: Traversable[Directory] = contents.filter(_.isDirectory).map(p => new Directory(p.jfile))
  def files: Traversable[File] = contents.filter(_.isFile).map(p => new File(p.jfile))
  def createTempFile(prefix: String, suffix: Option[String] = None): File = {
	try {
      val tjfile = jio.File.createTempFile(prefix, suffix.getOrElse(null), jfile)
      new File(tjfile)
	} catch {
      case se: SecurityException => {
    	val cause = new AccessDenied(self, Some(se))
    	throw new LocationCreationFailed(self, se.getMessage, Some(cause))
      }
      case ioe: jio.IOException => throw new LocationCreationFailed(self, ioe.getMessage, Some(ioe))
	}
  }
}

final class Directory(inFile: jio.File) extends Location(inFile) with DirectoryOps[Directory] {
  protected[io] def typeString = "directory"
  protected def toDirectory = this
  def rename(targetName: String, overwriteExisting: Boolean = false): Directory = {
	val nf = performRename(targetName, overwriteExisting)
	new Directory(nf)
  }
  def isFile = false
  def isDirectory = true
  def create(createParents: Boolean = true): Unit = createLocation(createParents, () => jfile.mkdir())
  def file(name: String): File = File(this, name)
  def directory(name: String): Directory = Directory(this, name)
  def path(name: String): Path = Path(this, name)
  //def recursiveDelete
}

object Directory {
  def apply(name: String): Directory = apply(current, name)
  def apply(parent: Directory, name: String) = new Directory(new jio.File(parent.jfile, name))
  def temp: Directory = new Directory(new jio.File(System.getProperty("java.io.tmpdir")))
  def home: Directory = new Directory(new jio.File(System.getProperty("user.home")))
  def current: Directory = new Directory(new jio.File(System.getProperty("user.dir")))
  def roots: Traversable[Directory] = jio.File.listRoots.map(new Directory(_))
}


//// Note: calling list() on a file (as in not a directory) returns null
////       calling list() on an empty directory returns an empty array
//
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



