package scalax.io

import org.junit._
import Assert._

class TestGlobMatcher {
  @Test 
  def mustMatchSimpleFileNames() {
     val globMatcher = new GlobMapper("test.file")
     assertEquals("Simple file names should match", true, globMatcher.matches("test.file"))
     assertEquals("Simple file names should not match", false, globMatcher.matches("test2.file"))
     assertEquals("Simple file names should not match directories", false, globMatcher.matches("test/file"))
     ()
  }

  @Test 
  def mustMatchSimpleFileAndDirectoryNames() {
     val globMatcher = new GlobMapper("tmp/test.file")
     assertEquals("Simple file/directory names should match", true, globMatcher.matches("tmp/test.file"))
     assertEquals("Simple file/directory names should not match", false, globMatcher.matches("tmp/test2.file"))
     assertEquals("Simple file/directory names should not match extra directories", false, globMatcher.matches("/tmp/two/file"))
     ()
  }

  @Test 
  def mustMatchGlobFileNames() {
     val globMatcher = new GlobMapper("*")
     assertEquals("Simple glob names should match", true, globMatcher.matches("test"))
     //assertEquals("Simple file names should not match", false, globMatcher.matches("test2.file"))
     assertEquals("Simple glob names should not match directories", false, globMatcher.matches("test/file"))
     ()
  }
  @Test 
  def mustMatchComplexGlobFileNames() {
     val globMatcher = new GlobMapper("*.test")
     assertEquals("Complex GlobFile names should match", true, globMatcher.matches("test.test"))
     assertEquals("Complex GlobFile names should not match", false, globMatcher.matches("test.file"))
     assertEquals("Complex GlobFile names should not match directories", false, globMatcher.matches("test/file"))
     ()
  }
   @Test 
  def mustMatchSimpleGlobFileAndDirectoryNames() {
     val globMatcher = new GlobMapper("tmp/*.file")
     assertEquals("Simple glob file/directory names should match", true, globMatcher.matches("tmp/test.file"))
     assertEquals("Simple glob file/directory names should match", true, globMatcher.matches("tmp/test2.file"))
     assertEquals("Simple glob file/directory names should not match", false, globMatcher.matches("dummy/test2.file"))
     assertEquals("Simple glob file/directory names should not match", false, globMatcher.matches("tmp/test2.file2"))
     assertEquals("Simple glob file/directory names should not match extra directories", false, globMatcher.matches("/tmp/two/file"))
     ()
  }
 
}
