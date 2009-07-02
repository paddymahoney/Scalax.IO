
package scalax.io

import resource._
import org.junit._
import Assert._

import _root_.java.{io => jio}

class TestFile {
  val cwd = new jio.File(System.getProperty("user.dir"))
  def withExistingFile(test: jio.File => Unit): Unit = {
    withNonExistentFile { tmpFile => 
      assertTrue("setup failed - could not create new test file", tmpFile.createNewFile())
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
  //1. Create a file for a file that exists
  @Test def testCreateFileWhenAlreadyExists() {
    withExistingFile { tmpFile =>
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
  //4. Test write options for both success and failure:
  //   (a) NewFile
  //       - success:  A file that does not previously exist is created
  //       - failure:  The file already exists
  @Test def createNewFile() {
    withNonExistentFile { tmpFile =>
      val f = File(tmpFile.getName())
      val s = f.outputStream(WriteOption.NewFile)
      try {
	s.write(1.asInstanceOf[Byte])
      } finally {
	s.close()
      }
    }
  }
  //   (b) NewTempFile
  //       - success: A file is created, written to, closed, and deleted
  //       - failure: The file already exists, so it can't be created
  //   (c) AppendToExisting
  //       - success: an existing file is opened and successfully added to
  //       - failure: attempting to append to a file that doesn't exist
  //       - failure: the path exists but is a directory
  //   (d) TruncateExisting
  //       - success: an existing file is opened and truncated
  //       - failure: the file does not already exist
  //       - failure: the path exists but is a directory
  //   (e) NewOrTruncate
  //       - success: an existing file is opened and truncated
  //       - success: a new file is created and written to
  //       - failure: the path exists but is a directory
  //   (f) NewOrAppend
  //       - sucesss: an existig file is opened and appended
  //       - success: a new file is created and written to
  //       - failure: the path exists but is a directory
  //    
}

//class TestDirectory {

//}

//class TestPath {

//}
