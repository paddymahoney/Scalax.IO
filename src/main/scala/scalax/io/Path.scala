package scalax.io

import collection.Traversable
import util.matching.Regex

trait PathFactory extends AbstractFileFactory {
  final type FileOps_P = Path
  final type DirOps_P = Path
  type FileOps_R <: FileOps_P
  type DirOps_R <: DirOps_P
  def apply(parent: Directory, name: String): FileOps_R
  def temp: FileOps_R
  def home: FileOps_R
  def current: FileOps_R
}

object Path extends PathFactory {
  type FileOps_R = Path
  type DirOps_R = Path
  val impl: PathFactory = JavaPath
  def apply(pathName: String) = impl(pathName)
  def apply(parent: Directory, name: String): Path = impl(parent, name)
  def apply(parent: DirOps_P, name: String): Path = impl(parent, name)
  def createTempFile(prefix: String, suffix: String, path: Path): Path = impl.createTempFile(prefix, suffix, path)
  def temp = impl.temp
  def home = impl.home
  def current = impl.current
}

trait Path extends Location with DirectoryOpsMixin with FileOpsMixin { self =>
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

  // directory operations...maybe this should be in a mixin instead
  /** the full contents of this directory tree */
  def tree: Traversable[Path]
  /** */
  def contents: Traversable[Path]

  /**
   * the length of the this <code>File</code> in bytes
   * @todo what should this return if the file does not exist?  java.io.File will return 0L,
   *       the same as for an empty file.
   */
  def length: Long
}

import java.{ io => jio }

object JavaPath extends PathFactory with JavaFileFactory {
  type FileOps_R = JavaPath
  type DirOps_R = JavaPath
  //TODO: refactor common code in JavaPath and JavaDirectory companion objects into a mixin
  def apply(file: jio.File) = new JavaPath(file)
  def apply(dir: Directory, name: String) = new JavaPath(new jio.File(JavaDirectory(dir).file, name))
  def temp = JavaPath(System.getProperty("temp.dir"))
  def current = JavaPath(System.getProperty("user.dir"))
  def home = JavaPath(System.getProperty("user.home"))
}

final class JavaPath(protected[io] val file: jio.File) extends Path with JavaFileMixin with JavaDirectoryMixin {
  def asDirectory = {
    if (isDirectory || !exists) new JavaDirectory(file)
    else throw new UnsupportedOperationException("this Path is already a file, cannot convert to a directory")
  }
  def asFile = {
    if (isFile || !exists) JavaFile(file)
    else throw new UnsupportedOperationException("this Path is already a directory, cannot convert to a file")
  }
  def asFileOrDirectory: Option[Either[JavaFile, JavaDirectory]] = {
    if (!exists) None
    else if (isDirectory) Some(Right(new JavaDirectory(file)))
    else Some(Left(JavaFile(file)))
  }
  def createNewFile() = if (file.createNewFile()) Some(JavaFile(file)) else None
  def createNewDirectory() = if (file.mkdirs()) Some(new JavaDirectory(file)) else None
  def isDirectory = file.isDirectory
  def isFile = file.isFile
}

