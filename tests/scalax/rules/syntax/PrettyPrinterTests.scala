// -----------------------------------------------------------------------------
//
//  Scalax - The Scala Community Library
//  Copyright (c) 2005-8 The Scalax Project. All rights reserved.
//
//  The primary distribution site is http://scalax.scalaforge.org/
//
//  This software is released under the terms of the Revised BSD License.
//  There is NO WARRANTY.  See the file LICENSE for the full text.
//
// -----------------------------------------------------------------------------

package scalax.rules.syntax

import scalax.testing._

object PrettyPrinterTests extends TestSuite("PrettyPrinter") {

val source = """
package a.b.c

/** my comment */
object Hello extends Application {
  println("Hello World!")
}"""
  
val html = """<br />
<span class="reservedId">package</span>&#160;a.b.c<br />
<br />
<span class="comment">/**&#160;my&#160;comment&#160;*/</span><br />
<span class="reservedId">object</span>&#160;Hello&#160;<span class="reservedId">extends</span>&#160;Application&#160;{<br />
&#160;&#160;println(<span class="literal">"Hello&#160;World!"</span>)<br />
}"""
  
  val prettyPrinter = new PrettyPrinter {
    def apply(source : String) = prettyPrintFor(compilationUnit)(input(source))
  }

  "CompilationUnit" is { 
    assertEq(source, html, prettyPrinter(source))
  }

}
