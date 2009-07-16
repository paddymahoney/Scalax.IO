package scalax.io

import java.{io => jio}

/**
 * TODO - Update this to work
 */
class GlobMapper(pattern : String) {
   val compiledPattern = GlobParser.compile(pattern).getOrElse(error("Failed to compile pattern!"))

   

   def matches(path : String) : Boolean = {
       Console.println("Checking [" + path + "] against matcher: " + compiledPattern)
       compiledPattern.matches(GlobParser.splitPath(path).toList)
   }
   def subPathCouldMatch(path : String) : Boolean = compiledPattern.subPathCouldMatch(GlobParser.splitPath(path))
}


import scala.util.parsing.combinator.RegexParsers

sealed trait GlobParsingItem {
   def subPathCouldMatch(fileNameParts : Seq[String]) : Boolean
   def matches(fileNameParts : Seq[String]) : Boolean
   var next : GlobParsingItem = _
}

object NullParsingItem extends GlobParsingItem {
   def subPathCouldMatch(fileNameParts : Seq[String]) : Boolean = false
   def matches(fileNameParts : Seq[String]) : Boolean = fileNameParts match {
     case Nil => true
     case "" :: Nil => true
     case _ => false
   }
   override def toString = "NullParsingItem"
}


case class DirectoryGlob() extends GlobParsingItem {
   def subPathCouldMatch(fileNameParts : Seq[String]) : Boolean = fileNameParts match {
     case "" :: _ => true
     case _ => false
   }
   def matches(fileNameParts : Seq[String]) : Boolean = {
      fileNameParts match {
          case Nil => false
          case "" :: x =>
               if(!next.matches(fileNameParts)) matches(x) else false
          case _ => false
      }
   }
   override def toString = "DirectoryGlob(" + next + ")"
}

case class FileGlob() extends GlobParsingItem {
  
  def subPathCouldMatch(fileNameParts : Seq[String]) : Boolean = fileNameParts match {
     case x :: y =>
           !(0 to x.length).map( idx => x.substring(idx) ).forall( str => !next.subPathCouldMatch( str :: y ) )
     case _ => false
  }
  def matches(fileNameParts : Seq[String]) : Boolean = fileNameParts match {
     case x :: y =>
           !(0 to x.length).map(idx => x.substring(idx)).forall( str => !next.matches( str :: y ) )
     case _ => false
  }
   override def toString = "FileGlob(" + next + ")"
}

case class NextDirectory() extends GlobParsingItem {
  def subPathCouldMatch(fileNameParts : Seq[String]) : Boolean = fileNameParts match {
      case "" :: x => next.subPathCouldMatch(x)
      case _ => false
   }
   def matches(fileNameParts : Seq[String]) : Boolean = fileNameParts match {
      case "" :: Nil => true
      case "" :: x => next.matches(x)
      case _ => false
   }
   override def toString = "NextDirectory(" + next + ")"
}

case class StringMatch(startingString : String) extends GlobParsingItem {
  def subPathCouldMatch(fileNameParts : Seq[String]) : Boolean = fileNameParts match {
       case x :: y if x.startsWith(startingString) => next.subPathCouldMatch(x.substring(startingString.length) :: y)
       case _ => false
  }
  def matches(fileNameParts : Seq[String]) : Boolean = fileNameParts match {
       case x :: y if x.startsWith(startingString) => next.matches(x.substring(startingString.length) :: y)
       case _ => false
  }
   override def toString = "StringMatch("+startingString+"," + next + ")"
}

object GlobParser extends RegexParsers {

  def compile(str : String) : Option[GlobParsingItem]= {
    parseAll(full_matcher, str) match {
      case Success(result, next) =>
	Some(result)
        //TODO - Error handling...
      case Failure(msg,next) => 
            //Console.println("at line " + next.pos.line + " column " + next.pos.column +":" + msg )
            None
      case Error(msg,next) => 
            //Console.println("at line " + next.pos.line + " column " + next.pos.column +":"+ msg )
            None
    }
  }

  def splitPath(path : String) : Seq[String] = {
      val normalized = path.replaceAll(jio.File.pathSeparator, "/")
      normalized.split("/")
  }


  def full_matcher : Parser[GlobParsingItem] = rep(parsing_item)  ^^ { case list => 
     list.foldRight[GlobParsingItem](NullParsingItem) {  (l, r) =>
          l.next = r
          l
     }
  }

  def parsing_item = next_directory | directory_glob | file_glob | string_match

  def next_directory : Parser[GlobParsingItem] = "[\\\\/]".r ^^ { case _ => NextDirectory() }
  def directory_glob : Parser[GlobParsingItem] = "\\*\\*/".r ^^ { case _ => DirectoryGlob() }
  def file_glob : Parser[GlobParsingItem] = "\\*".r ^^ { case _ => FileGlob() }
  def string_match : Parser[GlobParsingItem] = "[^\\\\/\\*]+".r ^^ { case x => StringMatch(x) }
}
