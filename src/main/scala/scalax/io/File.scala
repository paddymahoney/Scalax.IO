package scalax.io

import java.nio.charset.Charset
import collection.Traversable
import util.matching.Regex
import _root_.scalax.resource.ManagedResource

/**
 * TODO - Update this to work
 */
class GlobMapper(pattern : String) {
   def matches(path : String) : Boolean = false
}

//TODO - Complete this interface!!!
trait File { self =>
    def isDirectory : Boolean
    def path : String
    def cannonicalPath : String
    def tree : Traversable[File]
    def /(path : String) : File
    def inputStream : InputStream
    def outputStream : OutputStream
    def copyTo(dest : File) : Unit
    def moveTo(dest : File) : Unit


    def pathFrom(file : File) : String = if(path.startsWith(file.path)) path.drop(file.path.length)  else path
    def /(glob : GlobMapper) = new Traversable[File] {
       def foreach[U](f : File => U) = self.tree.filter(x => glob.matches(x.pathFrom(self))).foreach(f)
    }
    def /(regex : Regex) = new Traversable[File] {
       def foreach[U](f : File => U) = self.tree.filter( x => regex.pattern.matcher(x.pathFrom(self)).matches).foreach(f)
    }



    def reader(implicit charset : Charset = Charset.forName("UTF-8")) = inputStream.reader(charset)
    def writer(implicit charset : Charset = Charset.forName("UTF-8")) = outputStream.writer(charset)
 
    def lines(implicit charset : Charset = Charset.forName("UTF-8"))  : Traversable[String] = ManagedStreams.lines(reader(charset).buffered)
    def chars(implicit charset : Charset = Charset.forName("UTF-8")) : Traversable[Char] = ManagedStreams.chars(reader(charset).buffered)
    def bytes : Traversable[Byte] = ManagedStreams.bytes(inputStream.buffered)
    def slurp : Array[Byte] = for(in <- ManagedResource(inputStream)) yield in.slurp

    def writeLines(lines : Iterable[String])(implicit charset : Charset = Charset.forName("UTF-8")) : Unit = for(out <- ManagedResource(writer)) {
          out.writeLines(lines)
    }
    def write(input : String)(implicit charset : Charset = Charset.forName("UTF-8")) : Unit = for(out <- ManagedResource(writer)) {
          out.writeChars(input)
    }
    def write(input : Array[Byte]) : Unit = for(out <- ManagedResource(outputStream)) {
          out.write(input)()
    }
}


import java.{io => jio}

abstract class JavaFileWrapper(file : jio.File) extends File {
   def isDirectory = file.isDirectory()
   def path = file.getPath
   def cannonicalPath = file.getCanonicalPath
   //def /(path : String) = new JavaFileWrapper(new jio.File(file, path))
   //def copyTo(dest : File) : Unit = JavaFileHelp.copy(file,dest)
   //def moveTo(dest : File) : Unit = JavaFileHelp.move(file,dest)
   //def tree : Traversable[File]
   def inputStream = new JavaInputStreamWrapper(new jio.FileInputStream(file))
   def outputStream = new JavaOutputStreamWrapper(new jio.FileOutputStream(file))

    
}

