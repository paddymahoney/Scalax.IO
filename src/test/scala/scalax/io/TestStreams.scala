package scalax.io

import scalax.resource._
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

   @Test
   def mustWriteLinesWithUnixStyle() {
          val jstream = new jio.StringWriter();
          val writerStream = new JavaWriterStreamWrapper(jstream, LineEndingStyle.UNIX)
          writerStream.writeLines(List(""))
          writerStream.close()
          val output = jstream.toString();
          assertTrue("Failed to output unix endline", output.endsWith("\n"))
   }

   @Test
   def mustWriteLinesWithMacStyle() {
          val jstream = new jio.StringWriter();
          val writerStream = new JavaWriterStreamWrapper(jstream, LineEndingStyle.MAC)
          writerStream.writeLines(List(""))
          writerStream.close()
          val output = jstream.toString();
          assertTrue("Failed to output unix endline", output.endsWith("\r"))
   }
   @Test
   def mustWriteLinesWithWindowsStyle() {
          val jstream = new jio.StringWriter();
          val writerStream = new JavaWriterStreamWrapper(jstream, LineEndingStyle.WINDOWS)
          writerStream.writeLines(List(""))
          writerStream.close()
          val output = jstream.toString();
          assertTrue("Failed to output unix endline", output.endsWith("\r\n"))
   }


   @Test
   def mustReadLinesWithUnixStyle() {
     val jstream = new jio.StringReader("Line 1\nLine 2");
     val readerStream = JavaConversions.readerStream(jstream)
     val lines = readerStream.lines(LineEndingStyle.UNIX).toList
     assertEquals("Failed to pull appropriate number of lines!",2,lines.length)
     assertEquals("Failed to read first line", "Line 1",lines(0))
     assertEquals("Failed to read second line", "Line 2",lines(1))
     readerStream.close();
   }
   @Test
   def mustReadLinesWithMacStyle() {
     val jstream = new jio.StringReader("Line 1\rLine 2");
     val readerStream = JavaConversions.readerStream(jstream)
     val lines = readerStream.lines(LineEndingStyle.MAC).toList
     assertEquals("Failed to pull appropriate number of lines!",2,lines.length)
     assertEquals("Failed to read first line", "Line 1",lines(0))
     assertEquals("Failed to read second line", "Line 2",lines(1))
     readerStream.close();
   }
   @Test
   def mustReadLinesWithWindowsStyle() {
     val jstream = new jio.StringReader("Line 1\r\nLine 2");
     val readerStream = JavaConversions.readerStream(jstream)
     val lines = readerStream.lines(LineEndingStyle.WINDOWS).toList
     assertEquals("Failed to pull appropriate number of lines!",2,lines.length)
     assertEquals("Failed to read first line", "Line 1",lines(0))
     assertEquals("Failed to read second line", "Line 2",lines(1))
     readerStream.close();
   }

   @Test
   def mustNotReadUnixLinesWithMacStyle() {
     val line = "Line 1\nLine 2"
     val jstream = new jio.StringReader(line);
     val readerStream = JavaConversions.readerStream(jstream)
     val lines = readerStream.lines(LineEndingStyle.MAC).toList
     assertEquals("Failed to pull appropriate number of lines!",1,lines.length)
     assertEquals("Failed to read first line", line, lines(0))
     readerStream.close();
   }

   @Test
   def mustNotReadMacLinesWithUnixStyle() {
     val line = "Line 1\rLine 2"
     val jstream = new jio.StringReader(line);
     val readerStream = JavaConversions.readerStream(jstream)
     val lines = readerStream.lines(LineEndingStyle.UNIX).toList
     assertEquals("Failed to pull appropriate number of lines!",1,lines.length)
     assertEquals("Failed to read first line", line, lines(0))
     readerStream.close();
   }

   @Test
   def mustNotReadAllWindowsLineEndingWithUnixStyle() {
     val line = "Line 1\r\nLine 2"
     val jstream = new jio.StringReader(line);
     val readerStream = JavaConversions.readerStream(jstream)
     val lines = readerStream.lines(LineEndingStyle.UNIX).toList
     assertEquals("Failed to pull appropriate number of lines!",2,lines.length)
     assertEquals("Failed to read first line", "Line 1\r", lines(0))
     assertEquals("Failed to read second line", "Line 2", lines(1))
     readerStream.close();
   }

   @Test
   def mustNotReadAllWindowsLineEndingWithMacStyle() {
     val line = "Line 1\r\nLine 2"
     val jstream = new jio.StringReader(line);
     val readerStream = JavaConversions.readerStream(jstream)
     val lines = readerStream.lines(LineEndingStyle.MAC).toList
     assertEquals("Failed to pull appropriate number of lines!",2,lines.length)
     assertEquals("Failed to read first line", "Line 1", lines(0))
     assertEquals("Failed to read second line", "\nLine 2", lines(1))
     readerStream.close();
   }

   @Test
   def mustNotReadMacLinesWithWindowsStyle() {
     val line = "Line 1\rLine 2"
     val jstream = new jio.StringReader(line);
     val readerStream = JavaConversions.readerStream(jstream)
     val lines = readerStream.lines(LineEndingStyle.WINDOWS).toList
     assertEquals("Failed to pull appropriate number of lines!",1,lines.length)
     assertEquals("Failed to read first line", line, lines(0))
     readerStream.close();
   }

   @Test
   def mustNotReadUnixLinesWithWindowsStyle() {
     val line = "Line 1\nLine 2"
     val jstream = new jio.StringReader(line);
     val readerStream = JavaConversions.readerStream(jstream)
     val lines = readerStream.lines(LineEndingStyle.WINDOWS).toList
     assertEquals("Failed to pull appropriate number of lines!",1,lines.length)
     assertEquals("Failed to read first line", line, lines(0))
     readerStream.close();
   }
}
