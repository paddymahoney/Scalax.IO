package scalax.io

import collection.Traversable
import util.matching.Regex
import _root_.scalax.resource.ManagedResource

import scala.io.Codec

/** stuff that should be in an object somewhere.... */
trait Stuff {  
  def roots: Traversable[Location]
}

trait FileFactory {
  def apply(name: String): File
  def apply(name: String, dir: Directory): File
  def createTempFile(prefix: String = "", suffix: String = "", dir: Directory = Directory.tempDirectory): File
}

object File extends FileFactory {
  val impl: FileFactory = JavaFile
  final val extensionRegex = """^.*\.([^.]+)$""".r
  def apply(name: String): File = impl(name)
  def apply(name: String, dir: Directory): File = impl(name, dir)
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

private[io] trait FileOpsMixin extends Location { self =>
  def inputStream: InputStream
  def outputStream(opt: WriteOption = WriteOption.defaultWriteOption): OutputStream
  /**
   * @return the length of the this file in bytes, 0L for a non-existent file
   */
  def length: Long
  def reader(implicit codec: Codec = Codec.default) = inputStream.reader(codec)
  def writer(implicit codec: Codec = Codec.default) = outputStream().writer(codec)

  def lines(implicit codec: Codec = Codec.default) : Traversable[String] = ManagedStreams.lines(reader(codec).buffered)(LineEndingStyle.ALL) //TODO - Figure out implicit/defaults issue!!!
  def chars(implicit codec: Codec = Codec.default) : Traversable[Char] = ManagedStreams.chars(reader(codec).buffered)
  def bytes: Traversable[Byte] = ManagedStreams.bytes(inputStream.buffered)
  def slurp: Array[Byte] = for(in <- ManagedResource(inputStream)) yield in.slurp

  def writeLines(lines: Iterable[String])(implicit codec: Codec = Codec.default) : Unit = for(out <- ManagedResource(writer)) {
        out.writeLines(lines)
  }
  def write(input : String)(implicit codec: Codec = Codec.default) : Unit = for(out <- ManagedResource(writer)) {
        out.writeChars(input)
  }
  def write(input : Array[Byte]) : Unit = for(out <- ManagedResource(outputStream())) {
        out.write(input)()
  }
  //TODO: toSource method for compatibility with scala.io
  def extension: Option[String] = name match {
    case File.extensionRegex(x) => Some(x)
    case _ => None
  }
}

import java.{ io => jio }

object JavaFile extends FileFactory {
  def apply(name: String): JavaFile = {
    val f = new jio.File(name)
    JavaFile(f)
  }
  def apply(name: String, dir: Directory): JavaFile = {
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
