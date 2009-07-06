package scalax.io

import collection.Traversable
import util.matching.Regex
import _root_.scalax.resource.ManagedResource

import scala.io.Codec

trait AbstractFileFactory {
  type FileOps_T <: FileOpsMixin // allow for both File and Path
  def apply(name: String): FileOps_T
  def apply(dir: Directory, name: String): FileOps_T
  def createTempFile(prefix: String = "tmp", suffix: String = ".tmp", dir: Directory = Directory.temp): FileOps_T
}

trait FileFactory extends AbstractFileFactory {
  type FileOps_T <: File
}

object File extends FileFactory {
  type FileOps_T = File
  val impl: FileFactory = JavaFile
  final val extensionRegex = """^.*\.([^.]+)$""".r
  def apply(name: String): File = impl(name)
  def apply(dir: Directory, name: String): File = impl(dir, name)
  def unapply(loc: Location): Option[File] = loc match {
    case file: File => Some(file)
    case path: Path if path.isFile || !path.exists => Some(path.asFile)
    case _ => None
  }
  def createTempFile(prefix: String, suffix: String, dir: Directory): File = impl.createTempFile(prefix, suffix, dir)
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
  type FileOps_T <: JavaFileMixin
}

object JavaFile extends JavaFileFactory with FileFactory {
  type FileOps_T = JavaFile
  def apply(name: String): JavaFile = {
    val f = new jio.File(name)
    JavaFile(f)
  }
  def apply(dir: Directory, name: String): JavaFile = {
    val jDir = JavaDirectory(dir).file
    val f = new jio.File(jDir, name)
    JavaFile(f)
  }
  def apply(file: jio.File): JavaFile = {
    if (file.isDirectory())
      throw new IllegalArgumentException("The specified location is an existing directory: " + file.getName())
    new JavaFile(file)
  }
  def createTempFile(prefix: String, suffix: String, dir: Directory): JavaFile = {
    val jDir: jio.File = JavaDirectory(dir).file
    val jFile = jio.File.createTempFile(prefix, suffix, jDir)
    new JavaFile(jFile)
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

final class JavaFile private (protected val file: jio.File) extends File with JavaFileMixin {
  def create() = file.createNewFile()
}
