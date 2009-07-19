
package scalax.io


import collection.Traversable
import util.matching.Regex
import java.{ io => jio }

trait DirectoryOps[T <: DirectoryOps[T]] extends Location { 
  self: T =>
  protected def make(f: jio.File): T
  protected def toDirectory: Directory
  def /(path: String): Location
  def /(glob: GlobMapper) = new Traversable[Location] {
    def foreach[U](f: Location => U) =
      self.tree.filter(x => glob.matches(x.pathFrom(self.toDirectory))).foreach(f)
  }
  def tree: Traversable[Path] = new Traversable[Path] {
     def foreach[U](f: Path => U) = (new JFileTree(file)).foreach(jf => f(new Path(jf)))
  }
}

final class Directory(inFile: jio.File) extends Location(inFile) with DirectoryOps[Directory] {
  protected def make(f: jio.File) = new Directory(f)
  protected[io] def typeString = "directory"
  protected def toDirectory = this
  def rename(targetName: String, overwriteExisting: Boolean = false): Directory = {
	val nf = performRename(targetName, overwriteExisting)
	new Directory(nf)
  }
  def isFile = false
  def isDirectory = true
  def /(name: String): Location = Path(this, name)
  def create(createParents: Boolean = true): Unit = createLocation(createParents, () => file.mkdir())
  //def recursiveDelete
}

object Directory {
  def apply(name: String): Directory = apply(current, name)
  def apply(parent: Directory, name: String) = new Directory(new jio.File(parent.file, name))
  //temp
  //home
  def current: Directory = new Directory(new jio.File(System.getProperty("user.dir")))
  //roots
}


//
//
//trait Directory extends Location with DirectoryOpsMixin { self =>
//  /** the subdirectories immediately under this directory */
//  def children: Traversable[Directory]
//  /** the <code>File</code>s within this <code>Directory</code>*/
//  def files: Traversable[File]
//  def contents: Traversable[Location]
//  /**
//   * atomically create new empty Location with this pathname if one does not exist
//   * @return true if a file was created, false if not
//   */
//  def create: Boolean
//  /** @return true */
//  final def isDirectory = true
//  /** @return false */
//  final def isFile = false
//  protected def asDirectory = self
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
//import java.{ io => jio }
//
//trait JavaDirectoryFactory extends AbstractDirectoryFactory {
//  type DirOps_R <: JavaDirectoryMixin
//  type FileOps_R <: JavaFileMixin
//  type Loc_R <: JavaDirectoryMixin
//  def apply(file: jio.File): Loc_R
//  def apply(name: String): Loc_R = apply(new jio.File(name))
//  def apply(dir: DirOps_P, name: String): Loc_R = apply(JavaDirectory(dir), name)
//  def temp: Loc_R = {
//    val tmpDirName = System.getProperty("java.io.tmpdir")
//    apply(tmpDirName)
//  }
//  def home: Loc_R = apply(System.getProperty("user.home"))
//  def current: Loc_R = apply(System.getProperty("user.dir"))
//  def roots: Traversable[Loc_R] = jio.File.listRoots.map(apply(_))
//}
//
//object JavaDirectory extends DirectoryFactory with JavaDirectoryFactory {
//  type DirOps_R = JavaDirectory
//  type FileOps_R = JavaFile
//  type Loc_R = JavaDirectory
//  //TODO: this duplicates extractFile in JavaFileFactory, move into JavaLocationFactory
//  protected def extractFile(loc: Location): jio.File = loc match {
//    case jloc: JavaLocation => jloc.file
//    case _ => new jio.File(loc.name) //TODO: should this use the absolute or canonical name instead?
//  }
//  def apply(file: jio.File): Loc_R = {
//    if (file.isFile()) throw new IllegalArgumentException("The specified location is an existing file: " + file.getName())
//    new JavaDirectory(file)
//  }
//}
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
//  def files: Traversable[FileOps_R] = file.listFiles.filter(_.isFile).map(JavaFile(_))
//  def newFile(name: String): FileOps_R = JavaFile(this, name)
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



