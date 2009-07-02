
package scalax.io

import resource._
import org.junit._
import Assert._

import _root_.java.{io => jio}

class TestFile {
  val cwd = new jio.File(System.getProperty("user.dir"))
  val empty = Array.empty[Byte]
  def withExistingFile(contents: Array[Byte] = empty)(test: jio.File => Unit): Unit = {
    withNonExistentFile { tmpFile => 
      assertTrue("setup failed - could not create new test file", tmpFile.createNewFile())
      if (contents.length > 0) {
	val s = new jio.FileOutputStream(tmpFile)
	try { s.write(contents) } finally { s.close() }
      }
      test(tmpFile)
    }
  }
  def withNonExistentFile(test: jio.File => Unit): Unit = {
    val tmpFile = new jio.File(cwd, "testfile.tmp")
    try {
      if (tmpFile.exists()) assertTrue("cleanup of pre-existing file failed", tmpFile.delete())
      test(tmpFile)
    } finally {
      if (tmpFile.exists()) {
	assertTrue("test teardown failed - could not delete temp file", tmpFile.delete())
      }
    }
  }
  def withExistingDirectory(test: jio.File => Unit): Unit = {
    withNonExistentFile { tmpDir =>
      assertTrue("setup failed - could not make a new directory", tmpDir.mkdir())
      test(tmpDir)
    }
  }
  def checkContents(file: jio.File, expected: Array[Byte]) {
    val is = new jio.FileInputStream(file)
    val len = expected.length + 10
    val buf = new Array[Byte](len)
    def read(pos: Int): Int = {
      if (pos == len) len
      else {
	val r = is.read(buf, pos, (len - pos))
	if (r < 0) pos else read(pos + r)
      }
    }
    val r = try { read(0) } finally { is.close() }
    assertEquals("the file did not have the expected length", r, expected.length)
    def compare(i: Int): Boolean = {
      if (i == r) true
      else if (expected(i) == buf(i)) compare(i + 1)
      else false
    }
    assertTrue("the file did not have the expected contents: expected: " +
	       expected.deepMkString(", ") + " actual: " + buf.slice(0, r).deepMkString(", "), compare(0))
  }
  //1. Create a file for a file that exists
  @Test def testCreateFileWhenAlreadyExists() {
    withExistingFile() { tmpFile =>
      val f = File(tmpFile.getName())
      assertTrue("failed to recognize existence of file", f.exists)
      assertFalse("f.create() should have returned false because file already exists", f.create())
    }
  }
  //2. Create a file that doesn't exist
  @Test def testCreateFileWhenDoesNotExist() {
    withNonExistentFile { tmpFile =>
      val f = File(tmpFile.getName())
      assertTrue("file creation failed", f.create())
    }
  }
  //3. Try to create a file with a pathname representing an existing directory, expect failure
  @Test(expected=classOf[IllegalArgumentException]) def testAttemptCreateFileThatIsExistingDirectory() {
    withExistingDirectory { existingDir =>
      File(existingDir.getName())
    }
  }
  //4. Test write options for both success and failure:
  //   (a) NewFile
  //       - success:  A file that does not previously exist is created
  //       - failure:  The file already exists
  @Test def createNewFileOutputStream() {
    withNonExistentFile { tmpFile =>
      val f = File(tmpFile.getName())
      val s = f.outputStream(WriteOption.NewFile)
      try { s.write(1.toByte) } finally { s.close() }
      checkContents(tmpFile, Array(1.toByte))
    }
  }
  @Test(expected=classOf[FileAlreadyExists]) def createNewFileOutputStreamFail() {
    withExistingFile() { existingFile =>
      val f = File(existingFile.getName())
      val s = f.outputStream(WriteOption.NewFile)
      s.close()  //just in case we get this far
    }
  }
  //   (b) NewTempFile
  //       - success: A file is created, written to, closed, and deleted
  //       - failure: The file already exists, so it can't be created
  @Test def createNewTempFileOutputStream() {
    withNonExistentFile { file =>
      val f = File(file.getName())
      val s = f.outputStream(WriteOption.NewTempFile)
      s.write(1.toByte)
      s.close()
      assertFalse("the file should have been deleted when the output stream was closed", f.exists)
    }
  }
  @Test(expected=classOf[FileAlreadyExists]) def createNewTempFileOutputStreamFail() {
    withExistingFile() { existingFile =>
      val f = File(existingFile.getName())
      val s = f.outputStream(WriteOption.NewTempFile)
      s.close() // just in case we get this far
    }
  }
  //   (c) AppendToExisting
  //       - success: an existing file is opened and successfully appended to
  //       - failure: attempting to append to a file that doesn't exist
  @Test def appendToExistingFile() {
    withExistingFile(Array(1.toByte)) { existingFile =>
      val f = File(existingFile.getName())
      val origLen = f.length
      val s = f.outputStream(WriteOption.AppendToExisting)
      try { s.write(2.toByte) } finally { s.close() }
      checkContents(existingFile, Array(1.toByte, 2.toByte))
    }
  }
  @Test(expected=classOf[FileDoesNotExist]) def appendToExistingFileFail() {
    withNonExistentFile { nonExistentFile =>
      val f = File(nonExistentFile.getName())
      val s = f.outputStream(WriteOption.AppendToExisting)
      s.close()
    }
  }
  //   (d) TruncateExisting
  //       - success: an existing file is opened and truncated
  //       - failure: the file does not already exist
  @Test def truncateExistingFile() {
    withExistingFile(Array(1.toByte)) { existingFile =>
      val f = File(existingFile.getName())
      val s = f.outputStream(WriteOption.TruncateExisting)
      try { s.write(2.toByte) } finally { s.close() }
      checkContents(existingFile, Array(2.toByte))
    }
  }
  @Test(expected=classOf[FileDoesNotExist]) def truncateExistingFileFail() {
    withNonExistentFile { nonExistentFile =>
      val f = File(nonExistentFile.getName())
      val s = f.outputStream(WriteOption.TruncateExisting)
      s.close() // just in case
    }
  }
  //   (e) NewOrTruncate
  //       - success: an existing file is opened and truncated
  //       - success: a new file is created and written to
  @Test def newOrTruncateWithExisting() {
    withExistingFile(Array(1.toByte)) { existingFile =>
      val f = File(existingFile.getName())
      val s = f.outputStream(WriteOption.NewOrTruncate)
      s.write(2.toByte)
      s.close()
      checkContents(existingFile, Array(2.toByte))
    }
  }
  //   (f) NewOrAppend
  //       - sucesss: an existig file is opened and appended
  //       - success: a new file is created and written to
}

//class TestDirectory {

//}

//class TestPath {

//}
