package scalax.io

import _root_.scalax.resource.{ManagedResource,ManagedTraversable}

/**
 * A Stream of bytes which is pulled from a managed resource.   Iterating over the bytes is done through a 
 */
class ManagedByteStream[A <: InputStream](val resource : ManagedResource[A]) extends ManagedTraversable[Byte] {
  type Handle = A
  protected def iterator(v : A) : Iterator[Byte] = v.bytes
}

/**
 * A Stream of characters which is pulled from a managed resource.   Iterating over the bytes is done through a 
 */
class ManagedCharStream[A <: ReaderStream](val resource : ManagedResource[A]) extends ManagedTraversable[Char] {
  type Handle = A
  protected def iterator(v : A) : Iterator[Char] = v.chars
}

/**
 * A Stream of characters which is pulled from a managed resource.   Iterating over the bytes is done through a 
 */
class ManagedLineStream[A <: ReaderStream](val resource : ManagedResource[A], lineEnding : LineEndingStyle.LineEndingStyle = LineEndingStyle.current_platform_style) extends ManagedTraversable[String] {
  type Handle = A
  protected def iterator(v : A) : Iterator[String] = v.lines
  
}

/**
 * A Stream of objects which is deserialized from a managed resource.   Iterating over the objects is done through a ManagedTraversable 
 */
class ManagedObjectStream[A <: ObjectInputStream](val resource : ManagedResource[A]) extends ManagedTraversable[Any] {
  type Handle = A
  protected def iterator(v : A) : Iterator[Any] = v.objects
  
}


/**
 * This module contains convienience methods for making managed streams.
 */
object ManagedStreams {

   def lines[A <: ReaderStream](input : => A)(implicit lineEnding : LineEndingStyle.LineEndingStyle = LineEndingStyle.current_platform_style) = new ManagedLineStream(ManagedResource(input), lineEnding)

   def chars[A <: ReaderStream](input : => A) = new ManagedCharStream(ManagedResource(input))

   def bytes[A <: InputStream](input : => A) = new ManagedByteStream(ManagedResource(input))
   
   def objects[A <: ObjectInputStream](input : => A) = new ManagedObjectStream(ManagedResource(input))

}
