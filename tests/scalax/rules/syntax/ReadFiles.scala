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

import java.io.{BufferedReader, File, FileReader, Reader}


object ReadFiles extends SimpleScalaParser with Application {
  //process(new File("src/scalax/rules"))
  process(new File("../scala-trunk/src/compiler"))
  //process(new File("tests/scalax/rules/scala/TestIncrementalScalaParser.scala"))
  //process(new File("../scala-trunk/src/compiler/scala/tools/nsc/symtab/Types.scala"))
  
  def process(file : File) {
    if (file.isDirectory) file.listFiles().foreach(process)
    else if (file.getName.endsWith(".scala")) read(file)
  }
  
  def read(file : File) {
    print(file + "...")
    val buffer = new StringBuffer()
    val reader = new BufferedReader(new FileReader(file))
    var line = ""
    while ({ line = reader.readLine(); line } != null) buffer append line append "\n"
    reader.close();
    val result = compilationUnit(input(buffer.toString))
    result match {
      //case Success(value, rest) => println(value + "\nRemaining = \"" + rest)//.mkString("") + "\"")
      case Success(rest : Iterable[Char], value) if rest.mkString("") != "" => error(value + "\nRemaining = \"" + rest.mkString("") + "\"")
      case Success(rest, value) => println("Success!")
      case _ => error("Failure!")
    }
  }
}
