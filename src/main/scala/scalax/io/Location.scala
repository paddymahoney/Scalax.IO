
package scalax.io

/**
 * A point on a file system, such as a file, directory, or abstract pathname
 */
trait Location { self =>
  def canRead: Boolean
  def canWrite: Boolean
  def exists: Boolean
  def name: String
  def isAbsolute: Boolean
  def isHidden: Boolean
  /**
   * the number of seconds since the Epoch when this <code>Location</code was modified
   * @todo what should be returned if the file does not exist?
   */
  def lastModified: Long

  /**
   * true if this <code>Location</code> represents a directory
   * Note the small "d" in "directory," this location may be a <code>Path</code>
   * which can represent either a file or directory on the filesystem, but is neither
   * a <code>File</code> or <code>Directory</code> object.
   */
  def isDirectory: Boolean
  /** true if this <code>Location</code> represents a file */
  def isFile: Boolean
  /** the directory containing this <code>Location</code> */
  def parent: Option[Directory]
  def path: String
  def cannonicalPath: String
  def delete(): Boolean
  def renameTo(name: Location): Boolean
  /**
   * @return path to this <code>Location</code> relative to the specified <code>Directory</code>
   */
  def pathFrom(dir: Directory): String = {
    if (cannonicalPath.startsWith(dir.cannonicalPath)) {
      path.drop(dir.cannonicalPath.length)
    } else cannonicalPath
  }
}
