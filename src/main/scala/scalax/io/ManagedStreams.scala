package scalax.io

import _root_.scalax.resource.{ManagedResource,ManagedTraversible}

/**
 * A Stream of bytes which is pulled from a managed resource.   Iterating over the bytes is done through a 
 */
class ManagedByteStream[A <: InputStream](val resource : ManagedResource[A]) extends ManagedTraversible[Byte] {
  type Handle = A
  protected def iterator(v : A) : Iterator[Byte] = v.bytes.iterator
}

/**
 * A Stream of characters which is pulled from a managed resource.   Iterating over the bytes is done through a 
 */
class ManagedCharStream[A <: ReaderStream](val resource : ManagedResource[A]) extends ManagedTraversible[Char] {
  type Handle = A
  protected def iterator(v : A) : Iterator[Char] = v.chars.iterator
}

/**
 * A Stream of characters which is pulled from a managed resource.   Iterating over the bytes is done through a 
 */
class ManagedLineStream[A <: ReaderStream](val resource : ManagedResource[A])(implicit lineEnding : LineEndingStyle.LineEndingStyle = LineEndingStyle.current_platform_style) extends ManagedTraversible[String] {
  type Handle = A
  protected def iterator(v : A) : Iterator[String] = v.lines.iterator
  
}


/**
 * This module contains convienience methods for making managed streams.
 */
object ManagedStreams {

   def lines[A <: ReaderStream](input : => A)(implicit lineEnding : LineEndingStyle.LineEndingStyle = LineEndingStyle.current_platform_style) = new ManagedLineStream(ManagedResource(input))(lineEnding)

   def chars[A <: ReaderStream](input : => A) = new ManagedCharStream(ManagedResource(input))

   def bytes[A <: InputStream](input : => A) = new ManagedByteStream(ManagedResource(input))

}
