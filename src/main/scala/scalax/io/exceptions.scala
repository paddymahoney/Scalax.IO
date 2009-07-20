
package scalax.io


import java.{ io => jio }

object MsgUtils {
  def decodeParent(parent: Option[String]) = parent match {
	case None => " in a root directory"
	case Some(p) => " in '" + p + "'"
  }
  def parentPath(loc: Location): Option[String] = loc.parent match {
    case None => None
    case Some(parent) => Some(parent.path)
  }
  def accessDeniedMessage(name: String, parent: Option[String]) = {
    "Access denied to '" + name + "'" + decodeParent(parent)  
  }
  def locationDoesNotExistMsg(name: String, parent: Option[String]) = {
	"The location '" + name + "'" + decodeParent(parent) + " does not exist"
  }
  def renameFailedMsg(origName: String, targetName: String, parent: Option[String], reason: String) = {
	"Rename of '" + origName + "' to '" + targetName + decodeParent(parent) + " failed: " + reason
  }
  def locationAlreadyExistsMsg(name: String, parent: Option[String]) = {
	"operation failed because the location '" + name + "'" + decodeParent(parent) + " already exists."
  }
  def deletionFailedMsg(name: String, reason: String, parent: Option[String]) = {
	"deletion of '" + name + "'" + decodeParent(parent) + " failed: " + reason
  }
  def fileCreationFailedMsg(name: String, reason: String, parent: Option[String]) = {
    "creation of '" + name + "'" + decodeParent(parent) + " failed: " + reason
  }
  def pathIsADirectoryMsg(name: String, parent: Option[String]) = {
	"file operation on '" + name + "'" + decodeParent(parent) + " failed because it is an existing directory"  
  }
  def pathIsFileMsg(name: String, parent: Option[String]) = {
	"directory operation on '" + name + "'" + decodeParent(parent) + " failed because it is an existing file"   
  }
}

import MsgUtils._

/**
 * base for IO errors raised by <code>scalax.io</code>
 */
trait IOException extends jio.IOException {
  final def message = getMessage()
  /** the relative name of the <code>Location</code> generating the error*/
  val name: String
  /** the absolute path of the parent directory */
  val parent: Option[String]
  val cause: Option[Throwable]
  cause match {
    case Some(cause) => initCause(cause)
    case None =>
  }
}

class AccessDenied(val name: String, val parent: Option[String], val cause: Option[Throwable])
      extends jio.IOException(accessDeniedMessage(name, parent)) 
      with IOException {
  def this(loc: Location, cause: Option[Throwable]=None) = {
    this(loc.name, parentPath(loc), cause)
  }
}

class PathResolutionFailed(val name: String, val parent: Option[String], val cause: Option[Throwable] = None)
      extends jio.IOException("Could not resolve pathname '" + name + "'")
      with IOException

/**
 * creation of a new <code>Location</code> failed because it already existed
 */
class LocationAlreadyExists(val name: String, val parent: Option[String], 
							val cause: Option[Throwable])
      extends jio.IOException(locationAlreadyExistsMsg(name, parent)) 
      with IOException {
  def this(loc: Location, cause: Option[Throwable]=None) = this(loc.name, parentPath(loc), cause)
}

/** 
 * an operation failed because the <code>Location</code> does not exist on the filesystem
 */
class LocationDoesNotExist(val name: String, val parent: Option[String],
						   val cause: Option[Throwable])
      extends jio.FileNotFoundException(locationDoesNotExistMsg(name, parent))
      with IOException {
  def this(loc: Location, cause: Option[Throwable] = None) = {
    this(loc.name, parentPath(loc), cause)
  }
}

/**
 * @param name the original name of the location
 * @param targetName the intended new name of the location
 * @param reason explanation of why the rename failed 
 * @param parent the path of the directory containing the file
 * @param cause the underlying reason why the rename failed, such as 
 *              <code>AccessDenied</code> or <code>LocationDoesNotExist</code> 
 */
class RenameFailed(val name: String, val targetName: String, val reason: String,
		           val parent: Option[String], val cause: Option[Throwable])
	  extends jio.IOException(renameFailedMsg(name, targetName, parent, cause.toString)) {
  def this(loc: Location, targetName: String, reason: String, cause: Option[Throwable]) = {
	this(loc.name, targetName, reason, parentPath(loc), cause)
  }
}

class DeletionFailed(val name: String, val reason: String, val parent: Option[String],
		             val cause: Option[Throwable])
      extends jio.IOException(deletionFailedMsg(name, reason, parent)) 
      with IOException {
  def this(loc: Location, reason: String, cause: Option[Throwable] = None) {
	this(loc.name, reason, parentPath(loc), cause)
  }
}

class LocationCreationFailed(val name: String, val reason: String, val parent: Option[String],
		                     val cause: Option[Throwable])
	  extends jio.IOException(fileCreationFailedMsg(name, reason, parent))
	  with IOException {
  def this(loc: Location, reason: String, cause: Option[Throwable] = None) = {
	this(loc.name, reason, parentPath(loc), cause)
  }
}

/**
 * thrown when a file operation is attempted on a <code>Path</code> that is an existing directory
 */
class PathIsADirectory(val name: String, val parent: Option[String], val cause: Option[Throwable])
      extends jio.IOException(pathIsADirectoryMsg(name, parent))
      with IOException {
  def this(p: Path, cause: Option[Throwable] = None) = this(p.name, parentPath(p), cause)
}

/**
 * 
 */
class PathIsAFile(val name: String, val parent: Option[String], val cause: Option[Throwable])
	  extends jio.IOException(pathIsFileMsg(name, parent))
	  with IOException {
  def this(p: Location, cause: Option[Throwable] = None) = this(p.name, parentPath(p), cause)
}
