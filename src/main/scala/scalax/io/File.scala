package scalax.io

import collection.Traversable
import util.matching.Regex
import _root_.scalax.resource.ManagedResource

import scala.io.Codec

import java.{ io => jio }

private[io] trait FileOps[T <: FileOps[T]] extends Location {
  self: T =>
  protected def toFile: File
  protected def toPath: Path
  /**
   * @return a new <code>InputStream</code>
   * @throws PathIsADirectory if the path already exists and is a directory
   * @throws LocationDoesNotExist if the file does not exist
   * @throws AccessDenied if the file could not be opened for reading
   */
  def inputStream: InputStream = {
    try {
      try {
        val is = new jio.FileInputStream(jfile)
	    new JavaInputStreamWrapper(is)
      } catch {
    	// the nested try/catch is used because file.isDirectory can throw a SecurityException
	    case fnf: jio.FileNotFoundException => {
		  // FileInputStream will throw a FileNotFoundException if the file does not exist
		  // or the file is a directory, so we need to check if the file is a directory
		  // in order to disambiguate between the two situations
		  // http://java.sun.com/j2se/1.5.0/docs/api/java/io/FileInputStream.html#FileInputStream(java.io.File)
		  if (jfile.isDirectory) {
			// this can occur even if this is a File object because the underlying filesystem
			// can be changed after the File object is instantiated
		    throw new PathIsADirectory(self.toPath, Some(fnf))
		  } else {
		    throw new LocationDoesNotExist(self, Some(fnf))
		  }
		}    
      }
	} catch {
	  case se: SecurityException => throw new AccessDenied(self, Some(se))
    }
  }
  /**
   * @param opt the <code>WriteOption</code> to use when opening this file
   * @return a new <code>OutputStream</code> for this file
   * @throws LocationDoesNotExist if the file does not already exist and the <code>WriteOption</code>
   *                              dictates that it should not be created
   * @throws LocationAlreadyExists if the file already exists and the <code>WriteOption</code>
   * @throws LocationCreationFailed if the new file could not be created
   * @throws AccessDenied if the file cannot be opened for writing
   */
  def outputStream(opt: WriteOption = WriteOption.defaultWriteOption): OutputStream = {
    try {
      if (jfile.exists) {
        if (!opt.openExisting) throw new LocationAlreadyExists(this)
      } else {
        if (!opt.createNew) throw new LocationDoesNotExist(this)
      }
      try {
        val s = new jio.FileOutputStream(jfile, opt.append)
        new JavaFileOutputStreamWrapper(opt, self.toFile, s)
      } catch {
    	// the two-level of catch are required because file.isDirectory and file.exists
    	// can both throw a SecurityException.
        case fnf: jio.FileNotFoundException => {
          // FileOutputStream will claim that the file could not be found for a variety of reasons,
      	  // most of which having nothing to do with whether the file could be found or not
      	  // http://java.sun.com/j2se/1.5.0/docs/api/java/io/FileOutputStream.html#FileOutputStream(java.io.File,%20boolean)
      	  if (jfile.isDirectory) {
      		// note that this can occur even if self is a File object, because changes
      		// can be made to the underlying file system after the File object is instantiated
      	    throw new PathIsADirectory(self.toPath, Some(fnf))
      	  } else if (!jfile.exists) {
      	    throw new LocationCreationFailed(self, fnf.getMessage, Some(fnf))
      	  } else {
      	    //TODO: this probably the wrong exception to throw, but I'm not sure how to
      	    //      distinguish the different situations that can lead to this point
      	    throw new AccessDenied(self, Some(fnf))
      	  }
        }    	  
      }
    } catch {
      case se: SecurityException => throw new AccessDenied(self, Some(se))
    }
  }
  /**
   * @return the length of the this file in bytes
   * @throws LocationDoesNotExist if this file does not exist on the filesystem
   * @throws AccessDenied if the file cannot be accessed
   */
  def length: Long = handleSecurity {
    val len = jfile.length
    if ((len > 0L) || exists) len
    else throw new LocationDoesNotExist(this)
  }
  /**
   * @param codec <code>Codec</code> to be used to decode file, defaults to <code>Codec.default</code>
   * @return a new <code>ReaderStream</code>
   * @throws PathIsADirectory if the path already exists and is a directory
   * @throws LocationDoesNotExist if the file does not exist
   * @throws AccessDenied if the file could not be opened for reading 
   * @todo should this return a FileReaderStream instead?
   */
  def reader(implicit codec: Codec = Codec.default): ReaderStream = inputStream.reader(codec)
  /**
   * @param codec <code>Codec</code> to be used to encode the file, defaults to <code>Codec.default</code>
   * @throws LocationDoesNotExist if the file does not already exist and the <code>WriteOption</code>
   *                              dictates that it should not be created
   * @throws LocationAlreadyExists if the file already exists and the <code>WriteOption</code>
   * @throws LocationCreationFailed if the new file could not be created
   * @todo should writer return a FileWriterStream instead of a WriterStream?
   */
  def writer(implicit codec: Codec = Codec.default): WriterStream = outputStream().writer(codec)

  /**
   * @param codec the <code>Codec</code> to be used to decode the file, defaults to <code>Codec.default</code>
   * @param lineEndingStyle the <code>LineEndingStyle</code> to be used in breaking the file into lines, default to <code>LineEndingStyle.ALL</code>
   * @return a new, lazy <code>Traversable[String]</code> containing the lines in the file
   * @throws PathIsADirectory if the path already exists and is a directory
   * @throws LocationDoesNotExist if the file does not exist
   * @throws AccessDenied if the file could not be opened for reading 
   */
  def lines(implicit codec: Codec = Codec.default, lineEndingStyle: LineEndingStyle.LineEndingStyle = LineEndingStyle.ALL): Traversable[String] = 
    ManagedStreams.lines(reader(codec).buffered)(lineEndingStyle)

  /**
   * @param codec the <code>Codec</code> to be used to decode the file, defaults to <code>Codec.default</code>
   * @return a lazily generated <code>Traversable[Char]</code> with all the characters in the file
   * @throws PathIsADirectory if the path already exists and is a directory
   * @throws LocationDoesNotExist if the file does not exist
   * @throws AccessDenied if the file could not be opened for reading
   * @todo if @specialize makes it into 2.8, make sure the returned Traversable is specialized
   */
  def chars(implicit codec: Codec = Codec.default): Traversable[Char] = ManagedStreams.chars(reader(codec).buffered)
  /**
   * @return a lazily generated <code>Tranversable[Byte]</code> contains the contents of the file
   * @throws PathIsADirectory if the path already exists and is a directory
   * @throws LocationDoesNotExist if the file does not exist
   * @throws AccessDenied if the file could not be opened for reading
   * @todo make sure the Traversable is specialized
   */
  def bytes: Traversable[Byte] = ManagedStreams.bytes(inputStream.buffered)
  /**
   * reads the entire contents of the file into a byte array
   * @return an <code>Array[Byte]</code> with the contents of the file
   * @throws PathIsADirectory if the path already exists and is a directory
   * @throws LocationDoesNotExist if the file does not exist
   * @throws AccessDenied if the file could not be opened for reading
   */
  def slurp: Array[Byte] = for(in <- ManagedResource(inputStream)) yield in.slurp

  /**
   * @param lines
   */
  def writeLines(lines: Iterable[String])(implicit codec: Codec = Codec.default): Unit = {
    for(out <- ManagedResource(writer)) {
      out.writeLines(lines)
	}
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

final class File(inFile: jio.File) extends Location(inFile) with FileOps[File] {
  if (jfile.isDirectory) throw new IllegalArgumentException(jfile.getName + " is an existing directory")
  protected[io] def typeString = "file"
  protected def toPath = new Path(jfile)
  protected def toFile: File = this
  def isFile = true
  def isDirectory = false
  /**
   * Creates a new file, optionally creating parent directories as well
   * @param createParents create parent directories if they don't already exist, default to true
   * @throws LocationCreationFailed if an error occurs, with details in <code>reason</code> and <code>cause</code>
   */
  def create(createParents: Boolean = true): Unit = {
	createLocation(createParents, () => jfile.createNewFile())
  }
  def rename(targetName: String, overwriteExisting: Boolean = false): File = new File(performRename(targetName, overwriteExisting))
}

object File {
  def apply(name: String): File = apply(Directory.current, name)
  def apply(parent: Directory, name: String): File = new File(new jio.File(parent.jfile, name))
  def createTempFile(prefix: String, suffix: String, dir: Directory): File = {
    val tmp = jio.File.createTempFile(prefix, suffix, dir.jfile)
    new File(tmp)
  }
  final val extensionRegex = """^.*\.([^.]+)$""".r
}



//trait FileFactory extends AbstractFileFactory {
//  final type FileOps_P = File
//  final type DirOps_P = Directory
//  type FileOps_R <: FileOps_P
//  type DirOps_R <: DirOps_P
//  type Loc_R <: File
//}
//
//object File extends FileFactory {
//  type FileOps_R = File
//  type DirOps_R = Directory
//  type Loc_R = File
//  val impl: FileFactory = JavaFile
//  def apply(name: String): File = impl(name)
//  def apply(dir: DirOps_P, name: String): File = impl(dir, name)
//  def unapply(loc: Location): Option[File] = loc match {
//    case file: File => Some(file)
//    case path: Path if path.isFile || !path.exists => Some(path.asFile)
//    case _ => None
//  }
//  def createTempFile(prefix: String, suffix: String, dir: DirOps_P): File = impl.createTempFile(prefix, suffix, dir)
//}
//
////TODO - Complete this interface!!!
//trait File extends Location with FileOpsMixin { self =>
//  final def isDirectory = false
//  final def isFile = true
//  /**
//   * atomically create new empty File with this pathname if one does not exist
//   * @return true if a file was created, false if not
//   */
//  def create(): Boolean
////  def copyTo(dest: File): Unit
////  def moveTo(dest: Location): Unit
//
//}
//
//private[io] trait FileOpsMixin extends Location { //self =>

//}
//
//import java.{ io => jio }
//
//trait JavaFileFactory extends AbstractFileFactory {
//  type FileOps_R <: JavaFileMixin
//  type DirOps_R <: JavaDirectoryMixin
//  type Loc_R <: JavaFileMixin
//  //protected def makeInstance(file: jio.File): FileOps_R
//  protected def extractFile(loc: Location): jio.File = loc match {
//    case jloc: JavaLocation => jloc.file
//    case _ => new jio.File(loc.name)  //TODO: should this use the absolute or canonical path?
//  }
//  def apply(name: String): Loc_R = {
//    val f = new jio.File(name)
//    apply(f)
//  }
//  def apply(dir: DirOps_P, name: String): Loc_R = {
//    val jDir = extractFile(dir)
//    val f = new jio.File(jDir, name)
//    apply(f)
//  }
//  def createTempFile(prefix: String, suffix: String, dir: DirOps_P): Loc_R = {
//    val jDir = extractFile(dir)
//    val jFile = jio.File.createTempFile(prefix, suffix, jDir)
//    apply(jFile)
//  }
//  def apply(file: jio.File): Loc_R
//}
//
//object JavaFile extends JavaFileFactory with FileFactory {
//  type FileOps_R = JavaFile
//  type DirOps_R = JavaDirectory
//  type Loc_R = JavaFile
//  def apply(file: jio.File): FileOps_R = {
//    if (file.isDirectory())
//      throw new IllegalArgumentException("The specified location is an existing directory: " + file.getName())
//    new JavaFile(file)
//  }
//}
//

//
//final class JavaFile private (protected[io] val file: jio.File) extends File with JavaFileMixin {
//  def create() = file.createNewFile()
//}
