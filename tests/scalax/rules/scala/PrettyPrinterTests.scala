package scalax.rules.scala

import scalax.testing._

object PrettyPrinterTests extends TestSuite("PrettyPrinter") {
  
val source = """
package a.b.c

/** my comment */
object Hello extends Application {
  println("Hello World!")
}"""
  
val html = """<br />
<span class="keyword">package</span>&nbsp;a.b.c<br />
<br />
<span class="comment">/**&nbsp;my&nbsp;comment&nbsp;*/</span><br />
<span class="keyword">object</span>&nbsp;Hello&nbsp;<span class="keyword">extends</span>&nbsp;Application&nbsp;{<br />
&nbsp;&nbsp;println(<span class="literal">"Hello&nbsp;World!"</span>)<br />
}"""
  
  val prettyPrinter = new PrettyPrinter[MemoisableStringInput] {
    def apply(source : String) = prettyPrintFor(compilationUnit)(new ScalaInput(new MemoisableStringInput(source)))
  }

  "CompilationUnit" is { 
    assertEq(source, html, prettyPrinter(source))
  }
  
}
