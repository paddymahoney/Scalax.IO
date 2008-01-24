package scalax.rules.scala;

class PrettyPrinter[T <: Input[Char, T] with Memoisable[T]] extends ScalaParser[T] {

  val index = position ^^ (_ index)
  def at(pos : Int) = index filter (_ == pos)
  
  val escapeItem : Rule[String] = (newline -^ "<br />\n"
    | ' ' -^ "&nbsp;"
    | '&' -^ "&amp;"
    | '<' -^ "&lt;"
    | item ^^ (_ toString))
    
  def escapeTo(pos : Int) = escapeItem *~- at(pos) ^^ toString
  
  def endStatement(allow : Boolean) = update(_.lastTokenCanEndStatement = allow)
  
  /** Look for a memoised result.  This is very ugly - try to think of a better way! */
  def recall(key : String) = (
      multiple(true) -~ endStatement(true) -~ memo(key, failure)
      | multiple(true) -~ endStatement(false) -~ memo(key, failure)
      | multiple(false) -~ endStatement(true) -~ memo(key, failure)
      | multiple(false) -~ endStatement(false) -~ memo(key, failure)) -~ index
      
  def escape(key : String) = ((recall(key) &) >> escapeTo &) ~- recall(key)
  def span(styleClass : String)(rule : Rule[String]) = rule ^^ ("<span class=\"" + styleClass + "\">" + _ + "</span>")
  def style(key : String) = span(key)(escape(key))
  
  val prettyPrint = (
        style("keyword") 
      | style("literal")
      | style("attributeValue")
      | style("comment")
      | style("xmlComment")
      | style("elementName")
      | style("attributeName")
      | span("xmlOther")(
            escape("startElement")
          | escape("emptyElement")
          | escape("tagEnd")
          | escape("endTag"))
      | escapeItem *) ^^ toString
      
  def prettyPrintFor(rule : Rule[Any]) = expect(((rule&) | none) -~ prettyPrint)
}

class MemoisableStringInput(val string : String, val index : Int) extends Input[Char, MemoisableStringInput] with DefaultMemoisable[MemoisableStringInput] {
  def this(string : String) = this(string, 0)

  lazy val next = if (index >= string.length) Failure
      else Success(string.charAt(index), new MemoisableStringInput(string, index + 1))
      
  //override protected def onSuccess[T](key : AnyRef,  result : Success[T, MemoisableStringInput]) { 
  //  println(key + " -> " + result) 
  //}
}
