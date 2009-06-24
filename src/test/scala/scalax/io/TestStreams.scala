package scalax.io

import resource._
import org.junit._
import Assert._

import _root_.java.{io => jio}

class TestStreams {

   @Test
   def mustDumpContents() {
      val input = """Some Really
really
really really
really, not all that big
content!!!!"""
      val readerResource = ManagedResource(JavaConversions.readerStream(new jio.StringReader(input)))

      val writer = new jio.StringWriter()
      val writerResource = ManagedResource(JavaConversions.writerStream(writer))       

      for(in <- readerResource; out <- writerResource) in >>> out

      assertEquals("Failed to dump input stream!", input, writer.toString)

   }
}
