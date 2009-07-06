package scalax.io

import org.junit._
import Assert._

import _root_.java.{io => jio}

class TestManagedStreams {

   @Test
   def mustReadLinesWithDefaultStyle() {
	val input = """Here is a line.
and another one!"""
        val jstream = new jio.StringReader(input)
        val readerStream = JavaConversions.readerStream(jstream)
        val lines = ManagedStreams.lines(readerStream)
        for( (line, idx) <- lines.toList.zipWithIndex) {
		val errMsg = "Line " + idx + " Failed to match!"
                idx match {
                        case 0 => assertEquals(errMsg, "Here is a line.", line)
                        case 1 => assertEquals(errMsg, "and another one!", line)
                        case _ => fail("Unknown line! "+idx+": "+ line)
                }
        }
        var closed = false
        try {
           jstream.ready()
        } catch {
             case e : jio.IOException => 
                closed = true
        }
        assertTrue("STream was not closed!", closed)
   }

  
}
