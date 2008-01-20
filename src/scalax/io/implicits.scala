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

package scalax.io
import java.io._

object Implicits {
	implicit def fileExtras(f : File) = new FileExtras(f)
	implicit def inputStreamExtras(s : InputStream) = new InputStreamExtras(s)
	implicit def readerExtras(r : Reader) = new ReaderExtras(r)
}
