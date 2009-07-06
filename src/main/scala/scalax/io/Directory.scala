
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
