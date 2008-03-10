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

import scalax.data._
import scalax.io._
import scalax.testing._
import java.io._

object FileHelpTests extends TestSuite("FileHelp") {
    "TmpDir" is {
        assert(FileHelp.tmpDir.exists)
    }
}

// vim: set ts=4 sw=4 et:
