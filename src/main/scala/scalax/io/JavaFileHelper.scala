package scalax.io

import java.nio.charset.Charset
import collection.Traversable
import util.matching.Regex
import _root_.scalax.resource.ManagedResource



import java.{io => jio}

class JavaFileWrapper(file : jio.File) extends File {
   def isDirectory = file.isDirectory()
   def path = file.getPath
   def cannonicalPath = file.getCanonicalPath
   def /(path : String) = new JavaFileWrapper(new jio.File(file, path))
   def tree : Traversable[File] = new Traversable[File] {
        def foreach[U](f : File => U) = (new JFileTree(file)).foreach(jf => f(new JavaFileWrapper(jf)))
   }
   def inputStream = new JavaInputStreamWrapper(new jio.FileInputStream(file))
   def outputStream = new JavaOutputStreamWrapper(new jio.FileOutputStream(file))

   def delete() : Boolean = file.delete()
   def renameTo(name : File) : Boolean = file.renameTo(new jio.File(name.cannonicalPath))
   def exists = file.exists()

}

object FileConstants {
  lazy val tmpDir = new JavaFileWrapper(new jio.File(System.getProperty("java.io.tmpdir")))
}


import scala.collection.mutable.Queue
/** Represents the preorder traversal of the directory tree rooted at the given file. */
class JFileTree(val root : jio.File) extends Iterable[jio.File] {
  def iterator = new Walk
 
  class Walk private[JFileTree] extends Iterator[jio.File] {
    private val files = new Queue[jio.File]();
    private val dirs = new Queue[jio.File]();
    private var failed = false;
    dirs.enqueue(root)
 
    private def enqueue(dir: jio.File) = {
      val list = dir.listFiles()
      if(list == null) {
        failed = true;
      } else {
        for (file <- dir.listFiles()) {
          if (file.isDirectory()) dirs.enqueue(file)
          else files.enqueue(file)
        }
      }
      dir
    }
 
    def hasNext = files.length != 0 || dirs.length != 0
 
    def next() = {
      failed = false;
      if (!hasNext) throw new NoSuchElementException("No more results")
      if (files.length != 0) files.dequeue else enqueue(dirs.dequeue)
    }
 
    def wasUnreadable = failed;
  }
}
