
package scalax.io


import java.io.{ IOException, FileNotFoundException}

class FileAlreadyExists(val fileName: String)
  extends java.io.IOException("The file '" + fileName + "' already exists.") {
  def message = getMessage
}

class FileDoesNotExist(val fileName: String)
      extends java.io.FileNotFoundException("The file '" + fileName + "' does not exist.") {
  def message = getMessage
}

class FileDeletionFailed(val fileName: String) extends IOException("Deletion of " + fileName + " failed.") {
  def message = getMessage
}
