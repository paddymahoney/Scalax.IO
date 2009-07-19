
package scalax.io

import java.{ io => jio }

/**
 * A point on a file system, such as a file, directory, or abstract pathname
 * Unlike java.io.Files, <code>Locations</code> are never relative.  They are
 * always either absolute or canonical because relative pathnames are resolved
 * into absolute pathnames on construction.
 * @param inFile java.io.File object used to generate the file that will be wrapped
 */
abstract class Location(inFile: jio.File) extends {
  protected[io] final val jfile: jio.File = try {
    inFile.getAbsoluteFile()
  } catch {
	case se: SecurityException => {
      val parent = inFile.getParent() match {
    	case null => None
    	case p => Some(p)
      }
      throw new PathResolutionFailed(name, parent, Some(se))
	}
  }
  protected def handleSecurity[R](block: => R): R = {
    val result: R = try {
      block	
    } catch {
      case se: SecurityException => throw new AccessDenied(this, Some(se))
    }
    result
  }
  /** returns name such as "file" or "directory" to be used in messages */
  protected[io] def typeString: String
  final def isReadable: Boolean = try {
	if (jfile.canRead) true        // the file exists and can be read
	else if (jfile.exists) false   // the file exists and cannot be read
	else parent match {           // the file does not exist...
      case Some(p) => p.isReadable// ...if it has a parent, see if it is readable
      case None => false          // ...if not, it must be a non-existent root, so it can't be read
	}
  } catch {
	case se: SecurityException => false
  }
  final def isWritable: Boolean = try {
    if (jfile.canWrite) true        // the file exists and can be written
    else if (jfile.exists) false    // the file exists and cannot be written
    else parent match {            // the file does not exist...
      case Some(p) => p.isWritable // ...so if it has a parent, see if the parent is writable
      case None => false           // ...and if not, it must be a non-existent root
    }
  } catch {
	case se: SecurityException => false
  }
  final def exists: Boolean = try {
	jfile.exists
  } catch {
	case se: SecurityException => throw new AccessDenied(this, Some(se))
  }
  final def name = jfile.getName()
  // files are always absolute
  //final def isAbsolute = file.isAbsolute
  final def isHidden = handleSecurity(jfile.isHidden)
  /**
   * true if this <code>Location</code> represents a directory
   * Note the small "d" in "directory," this location may be a <code>Path</code>
   * which can represent either a file or directory on the filesystem, but is neither
   * a <code>File</code> or <code>Directory</code> object.
   */
  def isDirectory: Boolean
  /** true if this <code>Location</code> represents a file */
  def isFile: Boolean
  /**
   * the number of seconds since the Epoch when this <code>Location</code was modified
   * @todo what should be returned if the file does not exist?
   */
  final def lastModified = handleSecurity(jfile.lastModified)
  /** the directory containing this <code>Location</code> */
  final lazy val parent: Option[Directory] = {
    jfile.getParentFile match {
      case null => None
      case dir => Some(new Directory(dir))
    }
  }
  /**
   * the absolute path to the file, including the name of the file
   */
  final def path = jfile.getPath
  final def cannonicalPath: String = handleSecurity(jfile.getCanonicalPath)
  def rename(targetName: String, overwriteExisting: Boolean = false): Location
  protected final def performRename(targetName: String, overwriteExisting: Boolean): jio.File = {
	try {
	  if (!exists) {
        val ldne = new LocationDoesNotExist(this, None)
        val reason = "this location does not exist and therefore cannot be renamed"
	    throw new RenameFailed(this, targetName, reason, Some(ldne))
	  }
	} catch {
	  case ad: AccessDenied => {
	    val reason = "access was denied to this location"
	    throw new RenameFailed(this, targetName, reason, Some(ad))
	  }
	}
	val newFile = new jio.File(jfile.getParent, name)
	if (!overwriteExisting) {
	  try {
		if (newFile.exists()) {
		  val reason = "a location with the target name already exists"
		  val lae = new LocationAlreadyExists(this)
		  throw new RenameFailed(this, targetName, reason, Some(lae))
		}
	  } catch {
		case se: SecurityException => {
		  val reason = "access was denied to the target location"
		  val ad = new AccessDenied(this, Some(se))
		  throw new RenameFailed(this, targetName, reason, Some(ad))
		}
	  }
	}
	val fileForRename = new jio.File(path, targetName)
	try {
	  if (newFile.renameTo(fileForRename)) {
		newFile
	  } else {
	    val reason = "rename of file failed for an unknown reason"
	    throw new RenameFailed(this, targetName, reason, None)
	  }
	} catch {
	  case se: SecurityException => {
		val reason = "access was denied to the target location"
		val ad = new AccessDenied(this, Some(se))
		throw new RenameFailed(this, targetName, reason, Some(ad))
	  }
	}
  }
  final override def toString = path
  /**
   * @return path to this <code>Location</code> relative to the specified <code>Directory</code>
   */
  def pathFrom(dir: Directory): String = {
    if (cannonicalPath.startsWith(dir.cannonicalPath)) {
      path.drop(dir.cannonicalPath.length)
    } else cannonicalPath
  }
  /**
   * @throws DeletionFailed if the file does not exist or could not be deleted
   */
  final def delete() {
	try {
	  if (!jfile.delete()) {
		// nothing was deleted, try to figure out why
		val (cause, reason) = if (!exists) {
		  val cause = new LocationDoesNotExist(this)
		  val reason = cause.message
		  (Some(cause), reason)
		} else if (jfile.isDirectory && jfile.listFiles.length > 0) {
		  (None, "the directory is not empty")
		} else if (!jfile.canWrite) {
		  val cause = new AccessDenied(this, None)
		  val reason = "the " + typeString + " is not writable"
		  (Some(cause), reason)
		} else (None, typeString + " deletion failed for an unknown reason")
		throw new DeletionFailed(this, reason, cause)
	  }
	} catch {
	  case se: SecurityException => {
		val ad = new AccessDenied(this, Some(se))
		val reason = ad.message
		throw new DeletionFailed(this, reason, Some(ad))
	  }
	}
  }
  //this does not belong here because it's meaning is ambiguous on a Path object
  //def create(creatParent: Boolean = true): Unit
  protected def createParentDirectories() {
    try {
	  parent match {
	    case Some(p) if (!p.exists) => p.create(true) 
	    case _ => // no parent to create
	  }
	} catch {
	  case lcf: LocationCreationFailed => {
	    val reason = typeString + " creation failed because parent directories could not be created"
	    throw new LocationCreationFailed(this, reason, Some(lcf))
	  }
	}
  }
  protected def createLocation(createParents: Boolean, performCreation: () => Boolean) {
    try {
      if (createParents) createParentDirectories()
      if (!performCreation()) {
		// file creation failed, figure out why...
		val (cause, reason) = if (jfile.exists()) {
		  val cause = new LocationAlreadyExists(this, None)
		  val reason = cause.message
		  (Some(cause), reason)
	    } else if (!createParents && parent.isDefined && !parent.get.exists) {
	      // if the parent is defined but does not exist, creation failed b/c
	      // the parent directory does not exist
	      val cause = new LocationDoesNotExist(parent.get)
	      val reason = typeString + " creation failed because the containing directory does not exist"
	      (Some(cause), reason)
		} else {
		  (None, typeString + " creation failed for an unknown reason")
		}
		throw new LocationCreationFailed(this, reason, cause)
	  }
    } catch {
	  case se: SecurityException => {
	    val cause = new AccessDenied(this)
	    val reason = cause.message
	    throw new LocationCreationFailed(this, reason, Some(cause))
	  }
	  case ioe: jio.IOException => {
		val reason = ioe.getMessage()
		throw new LocationCreationFailed(this, reason, Some(ioe))
	  }
	}
  }
}
