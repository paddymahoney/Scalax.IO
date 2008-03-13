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

package scalax.rules.syntax;

class PrettyPrinter extends SimpleScalaParser {

  val index = position ^^ (_())
  def at(pos : Int) = index filter (_ == pos)
  
  val escapeItem : Rule[String] = (scanner.newline -^ "<br />\n"
    | scanner.elem(' ') -^ "&#160;"
    | scanner.elem('&') -^ "&amp;"
    | scanner.elem('<') -^ "&lt;"
    | scanner.item ^^ (_ toString))
    
  def escapeTo(pos : Int) = escapeItem *~- at(pos) ^^ scanner.toString
  
  /** Look for a memoised result.  This is very ugly - try to think of a better way! */
  def recall(key : String) = (
      multiple(true) -~ lastTokenCanEndStatement(true) -~ createRule(key, failure)
      | multiple(true) -~ lastTokenCanEndStatement(false) -~ createRule(key, failure)
      | multiple(false) -~ lastTokenCanEndStatement(true) -~ createRule(key, failure)
      | multiple(false) -~ lastTokenCanEndStatement(false) -~ createRule(key, failure)) -~ index
      
  def escape(key : String) = ((recall(key) &) >> escapeTo &) ~- recall(key)
  def span(styleClass : String)(rule : Rule[String]) = rule ^^ ("<span class=\"" + styleClass + "\">" + _ + "</span>")
  def style(key : String) = span(key)(escape(key))
  
  val prettyPrint = (
        style("comment") 
      | style("keyword") 
      | style("literal")
      | style("attributeValue")
      | style("xmlComment")
      | style("elementName")
      | style("attributeName")
      | span("xmlOther")(
            escape("startElement")
          | escape("emptyElement")
          | escape("tagEnd")
          | escape("endTag"))
      | escapeItem *) ^^ scanner.toString
      
  def prettyPrintFor(rule : Rule[Any]) = expect(((rule&) | none) -~ prettyPrint)
  
}
