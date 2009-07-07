package scalax.io

import collection.Traversable
import util.matching.Regex
import _root_.scalax.resource.ManagedResource

import scala.io.Codec

trait AbstractFileFactory {
  /** parameter type for file-like objects */
  type FileOps_P <: FileOpsMixin // allow for both File and Path
  /** parameter type for directory-like objects */
  type DirOps_P <: DirectoryOpsMixin
  /** return type for file-like objects */
  type FileOps_R <: FileOpsMixin
  /** return type for directory-like objects */
  type DirOps_R <: DirectoryOpsMixin
  /**
   * construct a file from the specified <code>pathName</code>
   * The <code>pathName</code> may be relative, absolute, or canonical.
   * @param pathName the pathname for the file
   * @return a file object
   */
  def apply(pathName: String): FileOps_R
  /**
   * construct a file in the specified directory
   * @param parent the directory in which to place the object
   * @param name the pathname relative to <code>parent</code> for the file
   * @return a file within <code>parent</code>
   */
  def apply(parent: DirOps_P, name: String): FileOps_R
  //TODO: add default arguments to AbstractFileFactory.createTempFile
  def createTempFile(prefix: String, suffix: String, dir: DirOps_P): FileOps_R
}

trait FileFactory extends AbstractFileFactory {
  final type FileOps_P = File
  final type DirOps_P = Directory
  type FileOps_R <: FileOps_P
  type DirOps_R <: DirOps_P
}

object File extends FileFactory {
  type FileOps_R = File
  type DirOps_R = Directory
  val impl: FileFactory = JavaFile
  final val extensionRegex = """^.*\.([^.]+)$""".r
  def apply(name: String): File = impl(name)
  def apply(dir: DirOps_P, name: String): File = impl(dir, name)
  def unapply(loc: Location): Option[File] = loc match {
    case file: File => Some(file)
    case path: Path if path.isFile || !path.exists => Some(path.asFile)
    case _ => None
  }
  def createTempFile(prefix: String, suffix: String, dir: DirOps_P): File = impl.createTempFile(prefix, suffix, dir)
}

//TODO - Complete this interface!!!
trait File extends Location with FileOpsMixin { self =>
  final def isDirectory = false
  final def isFile = true
  /**
   * atomically create new empty File with this pathname if one does not exist
   * @return true if a file was created, false if not
   */
  def create(): Boolean
//  def copyTo(dest: File): Unit
//  def moveTo(dest: Location): Unit

}

private[io] trait FileOpsMixin extends Location { //self =>
  def inputStream: InputStream
  def outputStream(opt: WriteOption = WriteOption.defaultWriteOption): OutputStream
  /**
   * @return the length of the this file in bytes, 0L for a non-existent file
   */
  def length: Long
  def reader(implicit codec: Codec = Codec.default) = inputStream.reader(codec)
  def writer(implicit codec: Codec = Codec.default) = outputStream().writer(codec)

  def lines(implicit codec: Codec = Codec.default) : Traversable[String] = 
    ManagedStreams.lines(reader(codec).buffered)(LineEndingStyle.ALL) //TODO - Figure out implicit/defaults issue!!!

  def chars(implicit codec: Codec = Codec.default) : Traversable[Char] = ManagedStreams.chars(reader(codec).buffered)
  def bytes: Traversable[Byte] = ManagedStreams.bytes(inputStream.buffered)
  def slurp: Array[Byte] = for(in <- ManagedResource(inputStream)) yield in.slurp

  def writeLines(lines: Iterable[String])(implicit codec: Codec = Codec.default): Unit = for(out <- ManagedResource(writer)) {
    out.writeLines(lines)
  }
  def write(input: String)(implicit codec: Codec = Codec.default) : Unit = for(out <- ManagedResource(writer)) {
    out.writeChars(input)
  }
  def write(input: Array[Byte]) : Unit = for(out <- ManagedResource(outputStream())) {
    out.write(input)()
  }
  //TODO: toSource method for compatibility with scala.io
  def extension: Option[String] = name match {
    case File.extensionRegex(x) => Some(x)
    case _ => None
  }
}

import java.{ io => jio }

trait JavaFileFactory extends AbstractFileFactory {
  type FileOps_R <: JavaFileMixin
  type DirOps_R <: JavaDirectoryMixin
  //protected def makeInstance(file: jio.File): FileOps_R
  protected def extractFile(loc: Location): jio.File = loc match {
    case jloc: JavaLocation => jloc.file
    case _ => new jio.File(loc.name)  //TODO: should this use the absolute or canonical path?
  }
  def apply(name: String): FileOps_R = {
    val f = new jio.File(name)
    apply(f)
  }
  def apply(dir: DirOps_P, name: String): FileOps_R = {
    val jDir = extractFile(dir)
    val f = new jio.File(jDir, name)
    apply(f)
  }
  def createTempFile(prefix: String, suffix: String, dir: DirOps_P): FileOps_R = {
    val jDir = extractFile(dir)
    val jFile = jio.File.createTempFile(prefix, suffix, jDir)
    apply(jFile)
  }
  def apply(file: jio.File): FileOps_R
}

object JavaFile extends JavaFileFactory with FileFactory {
  type FileOps_R = JavaFile
  type DirOps_R = JavaDirectory
  def apply(file: jio.File): FileOps_R = {
    if (file.isDirectory())
      throw new IllegalArgumentException("The specified location is an existing directory: " + file.getName())
    new JavaFile(file)
  }
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

final class JavaFile private (protected[io] val file: jio.File) extends File with JavaFileMixin {
  def create() = file.createNewFile()
}
