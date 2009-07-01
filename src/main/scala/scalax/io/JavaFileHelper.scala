package scalax.io

import java.nio.charset.Charset
import collection.Traversable
import util.matching.Regex
import _root_.scalax.resource.ManagedResource



import java.{io => jio}

trait JavaLocation extends Location {
  protected val file: jio.File
  final def canRead = file.canRead
  final def canWrite = file.canWrite
  final def exists = file.exists
  final def name = file.getName()
  final def isAbsolute = file.isAbsolute
  final def isHidden = file.isHidden
  final def lastModified = file.lastModified
  final def parent: Option[JavaDirectory] = {
    file.getParentFile match {
      case null => None
      case dir => Some(new JavaDirectory(dir))
    }
  }
  final def path = file.getPath
  final def cannonicalPath = file.getCanonicalPath
  final def delete() = file.delete()
  def renameTo(name: Location) = name match {
    case jl: JavaLocation => file.renameTo(jl.file)
    case _ => file.renameTo(new jio.File(name.path))
  }
  final override def toString = path
}

private[io] trait JavaDirectoryMixin extends DirectoryOpsMixin with JavaLocation {
  def /(path : String) = new JavaPath(new jio.File(file, path))
  def tree: Traversable[JavaPath] = new Traversable[JavaPath] {
    def foreach[U](f: JavaPath => U) = (new JFileTree(file)).foreach(jf => f(new JavaPath(jf)))
  }
  def contents: Traversable[JavaPath] = file.listFiles.map(new JavaPath(_))
}

private[io] trait JavaFileMixin extends FileOpsMixin with JavaLocation {
  def length = file.length
  def inputStream = {
    // there aren't any read options other than Read, which really is implied
    new JavaInputStreamWrapper(new jio.FileInputStream(file))
  }
  def outputStream(opt: WriteOption = WriteOption.defaultWriteOption): OutputStream = {
    JavaFileOutputStreamWrapper(opt, file)
  }
}

final class JavaDirectory(protected val file: jio.File) extends Directory with JavaDirectoryMixin {
  def create = file.mkdirs
  def children: Traversable[JavaDirectory] = file.listFiles.filter(_.isDirectory).map(new JavaDirectory(_))
  def files: Traversable[JavaFile] = file.listFiles.filter(_.isFile).map(new JavaFile(_))
}

final class JavaFile(protected val file: jio.File) extends File with JavaFileMixin {
  def create() = file.createNewFile()
}

final class JavaPath(protected val file: jio.File) extends Path with JavaFileMixin with JavaDirectoryMixin {
  def asDirectory = {
    if (isDirectory || !exists) new JavaDirectory(file)
    else throw new UnsupportedOperationException("this Path is already a file, cannot convert to a directory")
  }
  def asFile = {
    if (isFile || !exists) new JavaFile(file)
    else throw new UnsupportedOperationException("this Path is already a directory, cannot convert to a file")
  }
  def asFileOrDirectory: Option[Either[JavaFile, JavaDirectory]] = {
    if (!exists) None
    else if (isDirectory) Some(Right(new JavaDirectory(file)))
    else Some(Left(new JavaFile(file)))
  }
  def createNewFile() = if (file.createNewFile()) Some(new JavaFile(file)) else None
  def createNewDirectory() = if (file.mkdirs()) Some(new JavaDirectory(file)) else None
  def isDirectory = file.isDirectory
  def isFile = file.isFile
}

object FileConstants {
  // EE: I made tmpDir a def rather than a lazy val because:
  //     (1) it returns a mutable object, and
  //     (2) someone could change the property "java.io.tmpdir"
  //     so I think you really want a new object every time
  def tmpDir = new JavaDirectory(new jio.File(System.getProperty("java.io.tmpdir")))
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
