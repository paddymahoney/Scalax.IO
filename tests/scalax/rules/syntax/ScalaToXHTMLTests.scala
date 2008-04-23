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
/*
object ScalaToXHTMLTests extends TestSuite("ScalaToXHTML") {
  
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
    
    val prettyPrinter = new ScalaToXHTML {
      def apply(source : String) = prettyPrintFor(compilationUnit)(input(source))
    }

    "CompilationUnit" is { 
      println(prettyPrinter(source))
      //assertEq(source, html, prettyPrinter(source))
    }
    
  }
*/