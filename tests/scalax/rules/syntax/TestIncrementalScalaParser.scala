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

package scalax.rules.syntax.test;

object TestIncrementalScalaParser extends ScalaParser[DefaultIncrementalInput] with Application {
  
  val incrementalInput = new DefaultIncrementalInput
  val input = new ScalaInput(incrementalInput)
  
  DefaultIncrementalInput.debug = true

  val line = memo("line", newline -^ "" | (!newline -~ item +) ~- (newline?) ^^ toString)
  val lines = view(line) _

  def printCompilationUnit() {
    println; println("Compilation Unit: ")
    println(compilationUnit(input))
  }
  
  def printLines() {
    println; println("Lines: ")
    println(lines(input).mkString("\n"))
  }

  // set up initial text
  incrementalInput.edit(0, 0, """
    package a.b.c
    
    /** my comment */
    object Hello extends Application {
      println("Hello World!")
    }
    """)

  //printLines()
  printCompilationUnit()

 // insert something
 incrementalInput.edit(19, 0, """
   class Dummy {
     val question = <element with="attribute">and some text</element>
     val answer = 42
   }
   """)

   //printLines()
   printCompilationUnit()
} 

