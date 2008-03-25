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

package scalax.rules.syntax.test

object TestScalaParser extends SimpleScalaParser with TestScanner {
  def Literal[T](value : T) = syntax.Literal(value)(NoPosition)
  
  DefaultMemoisable.debug = true
  
  def remaining(input : Input#State) = input.chars.drop(input.index).mkString("")

  def input(string : String) = Input(string, 0)(true, false)
  
  implicit def anyToElement[T](any : T) = new Element[T] {
    val value = any
    val start = 0
    val length = 0
  }

  def main(args : Array[String]) {
  //checkRule('this)("this" -> "this")
  
checkFailure(stableId)("A.bc.this")

checkRule(typeSpec)(
    "A" -> TypeDesignator(Nil, "A"),
    "A.B" -> TypeDesignator(List(Name("A")), "B"),
    "A.type" -> SingletonType(List(Name("A"))),
    "(A, \nB)" -> TupleType(List(TypeDesignator(Nil, "A"), TypeDesignator(Nil, "B"))),

    // why is this one so slow?
    "(A, )" -> TupleType(List(TypeDesignator(Nil, "A"))),

    "A#B[C, D]" -> ParameterizedType(TypeProjection(TypeDesignator(Nil, "A"))("B"))(List(TypeDesignator(Nil, "C"), TypeDesignator(Nil, "D"))),
    "A with B" -> CompoundType(TypeDesignator(Nil, "A"), List(TypeDesignator(Nil, "B")), None),
   "A => B" -> FunctionType(List(ParameterType(false, TypeDesignator(Nil, "A"), false)), TypeDesignator(Nil, "B")),
   "() => B" -> FunctionType(List(), TypeDesignator(List(), "B")),
    "(=> A, B*) => C" -> FunctionType(List(ParameterType(true, TypeDesignator(Nil, "A"), false), ParameterType(false, TypeDesignator(Nil, "B"), true)), TypeDesignator(Nil, "C")),
    "A B C" -> InfixType(TypeDesignator(Nil, "A"), "B", TypeDesignator(Nil, "C")),
    "A -: B -: C" -> InfixType(TypeDesignator(List(), "A"), "-:" , 
        InfixType(TypeDesignator(List(), "B"), "-:",TypeDesignator(List(), "C"))),
    "A @annot" -> AnnotatedType(TypeDesignator(List(), "A"), List(Annotation(TypeDesignator(List(), "annot"), List(), List()))),
    
    """A @annot("Yo!") { val name = "Fred" }""" -> AnnotatedType(TypeDesignator(List(), "A"), List(
        Annotation(TypeDesignator(List(), "annot"), List(List(Literal("Yo!"))), List(("name", Literal("Fred"))))))
 )
 
 checkRule(dcl)(
     "val a : A" -> ValDeclaration("a" :: Nil, TypeDesignator(Nil, "A")),
     "val a, b, c : A" -> ValDeclaration("a" :: "b" :: "c" :: Nil, TypeDesignator(Nil, "A")),
     "var a : A" -> VarDeclaration("a" :: Nil, TypeDesignator(Nil, "A")),
     "var a, b, c : A" -> VarDeclaration("a" :: "b" :: "c" :: Nil, TypeDesignator(Nil, "A")),
     
     "def a[B, C](b : => B, c : C*)(implicit d : D) : A" -> FunctionDeclaration("a",
         Some(List(
             TypeParameter("B",None,None,None,None), 
             TypeParameter("C",None,None,None,None))),
         List(List(
             Parameter(List(), "b", Some(ParameterType(true, TypeDesignator(List(),"B"), false))), 
             Parameter(List(), "c", Some(ParameterType(false, TypeDesignator(List(),"C"), true))))), 
         Some(List(
             Parameter(List(), "d", Some(ParameterType(false, TypeDesignator(List(),"D"), false))))),
         Some(TypeDesignator(List(),"A"))),
         
     "type A[+B <: C, -D >: E, F <% G] >: H <: I" -> TypeDeclaration("A", 
         Some(List(
             VariantTypeParameter(Covariant, TypeParameter("B", None, None, Some(TypeDesignator(List(), "C")), None)), 
             VariantTypeParameter(Contravariant, TypeParameter("D", None, Some(TypeDesignator(List(), "E")), None, None)), 
             VariantTypeParameter(Invariant, TypeParameter("F", None, None, None, Some(TypeDesignator(List(), "G")))))), 
         Some(TypeDesignator(List(), "H")),
         Some(TypeDesignator(List(), "I")))
     )
     
     checkRule(importStat)(
         "import A.B, C._" -> ImportStatement(List(
             Import(List(Name("A")), List(ImportSelector("B", None))), 
             Import(List(Name("C")), List(ImportSelector("_", None))))),
             
         "import A.{b => c, _}" -> ImportStatement(List(
             Import(List(Name("A")),List(
                 ImportSelector("b",Some("c")), 
                 ImportSelector("_",None)))))
     )
     
     checkRule(expr)(
         "\"string\"" -> Literal("string"),
         "'symbol" -> Literal('symbol),
         "_" -> Underscore,
         "(1, 2, )" -> TupleExpression(List(Literal(1), Literal(2))),
         "1 .toString" -> DotExpression(Literal(1), Name("toString")),
         
         "a[B, C]" -> ExpressionTypeArgs(Name("a"),
             List(TypeDesignator(List(), "B"), TypeDesignator(List(), "C"))),
             
         "a(1, 2)" -> ApplyExpression(Name("a"), List(Literal(1), Literal(2))),
         
         "if (a) 1 else 2" -> IfExpression(Name("a"),Literal(1),Some(Literal(2))),
         
         "while (true) println(\"Hello\")" -> WhileExpression(Literal(true), ApplyExpression(Name("println"),List(Literal("Hello")))),
         
         "do println(\"Hello\") while(true)" -> DoExpression(ApplyExpression(Name("println"),List(Literal("Hello"))), Literal(true)),
         
         "throw x" -> Throw(Name("x")),
         "return x" -> Return(Some(Name("x"))),
         "return" -> Return(None),
         
         "try { 1 } catch { case e => println(e) } finally { println(\"finally!\") }" -> TryCatchFinally(
             Block(List(), Some(Literal(1))),
             Some(CaseClauses(List(
                 CaseClause(VariablePattern("e"), None, Block(List(),
                     Some(ApplyExpression(Name("println"), List(Name("e"))))))))),
             Some(Block(List(), Some(ApplyExpression(Name("println"),List(Literal("finally!"))))))),
             
          "for (i <- list; val j = i; if true) yield j" -> ForComprehension(List(
              Generator(VariablePattern("i"), Name("list"), None), 
              ValEnumerator(VariablePattern("j"), Name("i")), 
              Guard(Literal(true))), 
              true, Name("j")),
              
          "a = 1" -> SimpleAssignment("a",Literal(1)),
          
          "a.b = 1" -> DotAssignment(Name("a"), "b", Literal(1)),
          
          "a(b) = 1" -> Update(Name("a"), List(Name("b")), Literal(1)),
          
          "a b" -> PostfixExpression(Name("a"),"b"),
          
          "1 + 2 * 3" -> InfixExpression("+", Literal(1), InfixExpression("*", Literal(2), Literal(3))),
          
          "-1" -> PrefixExpression("-", Literal(1)),
          
          "a _" -> Unapplied(Name("a")),
          
          "new X" -> InstanceCreation(ClassTemplate(None,Some(TypeDesignator(List(), "X")),List(),List(),None)),
          
          "new Y(1, 2) { val y = 3 }" -> InstanceCreation(ClassTemplate(
              None,
              Some(TypeDesignator(List(),"Y")),
              List(List(Literal(1), Literal(2))),
              List(),
              Some(TemplateBody(None, None,
                  List(AnnotatedDefinition(List(),List(),ValPatternDefinition(List(VariablePattern("y")),None,Literal(3)))))))),
                  
          """a match {
            case x : A
                if x == b =>
                    x
            case _ => b
          }""" -> MatchExpression(Name("a"),CaseClauses(List(
              CaseClause(TypedVariablePattern("x",TypeDesignator(List(), "A")),
                  Some(InfixExpression("==",Name("x"),Name("b"))),
                  Block(List(), Some(Name("x")))), 
              CaseClause(Underscore,None,Block(List(), Some(Name("b"))))))),

         "<foo bar='123' baz={456}><!--comment-->Some text{\"Hello XML\"}<empty/>&lt;notelement&gt;</foo>" -> NodeList(List(
             XMLElement("foo", List(
                 Attribute("bar",Literal("123")), 
                 Attribute("baz",Literal(456))))(
                 Some(NodeList(List(
                     XMLComment("comment"), 
                     TextNode("Some text"), 
                     Literal("Hello XML"), 
                     XMLElement("empty",List())(None),
                     TextNode("<notelement>")))))))
         )
     
     checkRule(pattern)(
         "_" -> Underscore,
         "1" -> Literal(1),
         "x" -> VariablePattern("x"),
         "X" -> Name("X"), //StableIdPattern(List(Name("X")), None, false),
         "x.y" -> DotExpression(Name("x"),Name("y")), //StableIdPattern(List(Name("x"), Name("y")), None, false),
         "X(a, b)" -> StableIdPattern(List(Name("X")), Some(List(VariablePattern("a"), VariablePattern("b"))),false),
         "X(_*)" -> StableIdPattern(List(Name("X")), Some(List()), true),
         "(x, y)" -> TupleExpression(List(VariablePattern("x"), VariablePattern("y"))),
         "a ~ b" -> InfixExpression("~", VariablePattern("a"), VariablePattern("b")),
         "a @ (x, y)" -> AtPattern("a",TupleExpression(List(VariablePattern("x"), VariablePattern("y")))),
         "a : A" -> TypedVariablePattern("a", TypeDesignator(List(), "A")),
         "_ : A" -> TypePattern(TypeDesignator(List(), "A")),
         "1 | 2" -> OrPattern(Literal(1), Literal(2))
     )
     
  checkRule(compilationUnit)("""
    package a.b
    
    class Hello {
      def hello() {
        println("Hello World")
      }
    }""" -> CompilationUnit(Some(List("a", "b")), List(
        AnnotatedDefinition(List(),List(),ClassDefinition(false, "Hello",None,List(),None, List(), None,
            ClassTemplate(None,None,List(),List(),Some(TemplateBody(None,None,List(
                AnnotatedDefinition(List(),List(),ProcedureDefinition("hello",None,List(List()),None,
                    Block(List(), Some(ApplyExpression(Name("println"),List(Literal("Hello World")))))))))))))))
  )
    
  /*
  checkRule(scanner.unicodeEscape)("\\u0030" -> '0', "\\u21D2" -> '\u21D2')
  checkRule(scanner.octalEscape)("\\061" -> '1')
  checkRule(scanner.anyChar)("\\u0030" -> '0', "\\u21D2" -> '\u21D2')
  checkRule(scanner.opChar)("\\u21D2" -> '\u21D2')
  
  checkFailure(scanner.integerLiteral)("l", "L", "0x")
  
  checkRuleWithRest(scanner.integerLiteral) (
      "0l" -> Literal(0) -> "",
      "12 " -> Literal(12) -> " ",
      "012" -> Literal(10) -> "",
      "0x12" -> Literal(18) -> "")
      
  checkFailure(scanner.opChar)(".", ";", "(", "[", "}")
  
  checkRule(scanner.opChar) (
      "+" -> '+',
      "-" -> '-',
      "*" -> '*',
      "/" -> '/')
   
      
  // check reserved words aren't ids
  checkFailure(id)(ScalaScanner.reserved.toList : _*)
  //checkFailure(id(false))(reservedOps.keys.toList : _*)
  
  //checkRule(keyword)(
  //    "abstract" -> "abstract",
  //    "_" -> "_")
  
  checkRule(id)(
      "`yield`" -> "yield", 
      "yield1" -> "yield1", 
      "yield_+" -> "yield_+",
      "`\\u21D2`" -> "\u21D2")

  checkRule(scanner.floatLiteral)(
      "1f" -> Literal(1), 
      "1.0F" -> Literal(1), 
      "1.e2F" -> Literal(100),
      ".12E3f" -> Literal(.12E3f),
      "1D" -> Literal(1), 
      "1.0" -> Literal(1), 
      "1e2" -> Literal(100),
      ".12E3D" -> Literal(.12E3))
  
  */
      
  println("ScalaParser tests passed")
}
}

