

package scalax.io


object WriteOption {
  /** Create a new file, fail if one already exists, leave in place on close */
  val NewFile: WriteOption = new WriteOption( 
    name = "NewFile",
    createNew = true,
    openExisting = false,
    append = false,
    deleteOnClose = false)
  /** Create a new file, fail if one already exists, delete on close */
  val NewTempFile: WriteOption = new WriteOption(
    name = "NewTempFile",
    createNew = true,
    openExisting = false,
    append = false,
    deleteOnClose = true
  )
  /** Open an existing file and append to it, fail if does not exist */
  val AppendToExisting: WriteOption = new WriteOption(
    name = "AppendToExisting",
    createNew = false,
    openExisting = true,
    append = true,
    deleteOnClose = false
  )
  /** Open an existing file and truncate it, fail if it does not exist */
  val TruncateExisting: WriteOption = new WriteOption( 
    name = "TruncateExisting",
    createNew = false,
    openExisting = true,
    append = false,
    deleteOnClose = false
  )
  /** Create a new file or truncate existing file */
  val NewOrTruncate: WriteOption = new WriteOption( 
    name = "NewOrTruncate",
    createNew = true,
    openExisting = true,
    append = false,
    deleteOnClose = false
  )

  /** Create a file file or append to existing file */
  val NewOrAppend: WriteOption = new WriteOption(
    name = "NewOrAppend",
    createNew = true,
    openExisting = true,
    append = true,
    deleteOnClose = false
  )
  val defaultWriteOption = NewOrTruncate
}

final class WriteOption private
    (val name: String,
     val createNew: Boolean,
     val openExisting: Boolean,
     val append: Boolean,
     val deleteOnClose: Boolean) {
  override def toString = name
}


  // File Open Options for Reading
  // 1. Open file, fail if does not exist
  // 2. Open file, create new empty file if does not exist


