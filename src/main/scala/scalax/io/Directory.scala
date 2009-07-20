
package scalax.io


import collection.Traversable
import util.matching.Regex
import java.{ io => jio }

private[io] trait DirectoryOps[T <: DirectoryOps[T]] extends Location { 
  self: T =>
  protected def toDirectory: Directory
  def /(name: String): Path = Path(self.toDirectory, name)
  def /(glob: GlobMapper) = new Traversable[Location] {
    def foreach[U](f: Location => U) =
      self.tree.filter(x => glob.matches(x.pathFrom(self.toDirectory))).foreach(f)
  }
  def tree: Traversable[Path] = new Traversable[Path] {
     def foreach[U](f: Path => U) = (new JFileTree(jfile)).foreach(jf => f(new Path(jf)))
  }
  def contents: Traversable[Path] = handleSecurity(jfile.listFiles.map(new Path(_)))
  def children: Traversable[Directory] = handleSecurity(jfile.listFiles.filter(_.isDirectory).map(new Directory(_)))
  def files: Traversable[File] = handleSecurity(jfile.listFiles.filter(_.isFile).map(new File(_)))
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


//
//
//trait Directory extends Location with DirectoryOpsMixin { self =>

//  /**
//   * Create a new file object within this directory (does not actually add the file to filesystem)
//   * @param name the name of the new <code>File</code>
//   * @return a new <code>File</code> object for a file within this directory
//   */
//  def newFile(name: String): File
//  /**
//   * Create a new subdirectory within this directory (does not actually add the directory to the filesystem)
//   * @param name the name of the new <code>Directory</code>
//   * @return a new <code>Directory</code> object for a directory within this <code>Directory</code>
//   */
//  def newDirectory(name: String): Directory
//  /**
//   * Create a new path within this directory (does not actually add path to the filesystem)
//   * @param name the name of the new <code>Path</code>
//   * @return a new <code>Path</code> object for a path within this <code>Directory</code<
//   */ 
//  def newPath(name: String): Path
//}
//
//private[io] trait DirectoryOpsMixin extends Location { self =>
//  protected def asDirectory: Directory
//  def /(path: String): Path  // this needs to be a Path instead of a Location so that several levels
//			     // can be specified together
//  def /(glob: GlobMapper) = new Traversable[Location] {
//    def foreach[U](f: Location => U) =
//      self.tree.filter(x => glob.matches(x.pathFrom(self.asDirectory))).foreach(f)
//  }
//  def /(regex: Regex) = new Traversable[Location] {
//    def foreach[U](f: Location => U) =
//      self.tree.filter( x => regex.pattern.matcher(x.pathFrom(self.asDirectory)).matches).foreach(f)
//  }
//  /** the full contents of this directory tree */
//  def tree: Traversable[Location]
//  //TODO: def deleteRecursively from scala.io.File
//}
//
//
//private[io] trait JavaDirectoryMixin extends DirectoryOpsMixin with JavaLocation {
//  def /(path : String) = new JavaPath(new jio.File(file, path))

//  def contents: Traversable[JavaPath] = file.listFiles.map(new JavaPath(_))
//}
//
//final class JavaDirectory(protected[io] val file: jio.File) extends Directory with JavaDirectoryMixin {
//  type FileOps_R = JavaFile
//  type DirOps_R = JavaDirectory
//  type Loc_R = JavaDirectory
//  def create = file.mkdirs()
//  def children: Traversable[Loc_R] = file.listFiles.filter(_.isDirectory).map(new JavaDirectory(_))

//  def newDirectory(name: String): Loc_R = JavaDirectory(this, name)
//  def newPath(name: String): JavaPath = JavaPath(this, name)
//}
//
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



