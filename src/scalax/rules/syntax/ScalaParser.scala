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

package scalax.rules.syntax

/** A parser for Scala source code.
  *
  * @author Andrew Foggin
  *
  * based on Scala Language Specification.
  */
trait ScalaParser extends Parsers[Token] with MemoisableRules { 

  type X = SyntaxError
  type SyntaxRule[+T] = Rule[T, SyntaxError]
  
  def nextChar : SyntaxRule[Char]
  
  /** rule that sets multiple statements status and returns the previous value */
  def multiple(allow : Boolean) : SyntaxRule[Boolean]
  def multipleStatementsAllowed : SyntaxRule[Any]
  def lastTokenCanEndStatement(value : Boolean) : SyntaxRule[Any]
  def lastTokenCanEndStatement : SyntaxRule[Any]
  
  object scanner extends ScalaXMLParser {
    type S = ScalaParser.this.S
    lazy val item = nextChar
    lazy val newlineAllowed = multipleStatementsAllowed -~ lastTokenCanEndStatement
    
    // Scala expressions embedded in XML
    lazy val scalaPattern = curly(singleStatement(patterns ^^ TupleExpression))
        
    // Note: changed by me to permit what scalac seems to permit
    // TODO - raise issue and resolve
    lazy val scalaExpr = blockExpr
  }

  val position : SyntaxRule[() => Int]
  def element[T](rule : SyntaxRule[T]) : SyntaxRule[Element[T]] = position ~ rule ~ position ^~~^ ScalaElement[T]
  
  lazy val item : SyntaxRule[Token] = scanner.token >> canEndStatement as "token"
  def canEndStatement(token : Token) = lastTokenCanEndStatement(scanner.canEndStatement(token)) -^ token
  
  lazy val nl : SyntaxRule[Token] = NewLineToken
  lazy val literal : SyntaxRule[Literal] = item ^^? { case l : Literal => l } as "literal"
  lazy val quoteId : SyntaxRule[String] = item ^^? { case QuoteId(name) => name }
  lazy val plainId : SyntaxRule[String] = item ^^? { case PlainId(name) => name }
  lazy val id = quoteId | plainId as "id"
  lazy val reservedId : SyntaxRule[String] = item ^^? { case ReservedId(name) => name }  as "keyword"
  
  /** Treat a String as a rule that matches the corresponding plain or reserved id */
  implicit def stringToToken(name : String) : SyntaxRule[String] = item ^^? {
    case PlainId(`name`) => name
    case ReservedId(`name`) => name
  }
  implicit def stringToTokenSeq(name : String) = seqRule(stringToToken(name))

  /** Treat a Char as a rule that matches the corresponding delimeter, plain or reserved id */
  implicit def charToToken(char : Char) : SyntaxRule[Token] = item ?? {
    case Delimiter(`char`) =>
    case PlainId(name) if name == char.toString => 
    case ReservedId(name) if name == char.toString => 
  }
  implicit def charToTokenSeq(char : Char) = seqRule(charToToken(char))

  def singleStatement[T](rule : SyntaxRule[T]) : SyntaxRule[T] = for (s <- multiple(false); t <- rule; _ <- multiple(s)) yield t
  def multipleStatements[T](rule : SyntaxRule[T]) : SyntaxRule[T] = for (s <- multiple(true); t <- rule; _ <- multiple(s)) yield t

  lazy val semi : SyntaxRule[Any] = (nl+) | ';' as "semi"
    
  def round[T](rule : SyntaxRule[T]) = '(' -~ singleStatement(rule) ~- ')'
  def square[T](rule : SyntaxRule[T]) = '[' -~ singleStatement(rule) ~- ']'
  def curly[T](rule : SyntaxRule[T]) = '{' -~ multipleStatements(rule) ~- '}'
      
  def idToken(string : String) : SyntaxRule[String] = plainId | reservedId filter (_ == string)
      
  lazy val `=>` = "=>" | "\u21D2"
      
  lazy val qualId = id+/'.'
  lazy val ids = id+/','
    
  lazy val path = pathElement+/'.'
  lazy val pathElement : SyntaxRule[PathElement] = (id ^^ Name
      | "super" -~ (square(id) ?) ^^ Super
      | "this" -^ This)
    
  /** StableId is a Path ending in an id */
  lazy val stableId = path filter (_ last match {
    case Name(_) => true
    case _ => false
  })
  
  lazy val typeSpec : SyntaxRule[Type] = functionType | existentialType | infixType
  lazy val existentialType = infixType ~- "forSome" ~ curly((typeDcl | valDcl)+/semi) ^~^ ExistentialType
  lazy val functionType = (functionParameters | simpleFunctionParameter) ~- `=>` ~ typeSpec ^~^ FunctionType
  lazy val functionParameters = round(parameterType*/',').filter(checkParamTypes)
  lazy val simpleFunctionParameter = infixType ^^ { t => List(ParameterType(false, t, false)) }
  lazy val parameterType = (`=>` -?) ~ typeSpec ~ ('*' -?) ^~~^ ParameterType
  
  /** Checks that only the last parameter in a list is repeated (*) */
  private def checkParamTypes(list : List[ParameterType]) : Boolean = list match {
    case ParameterType(_, _, true) :: rest :: Nil => false
    case first :: rest => checkParamTypes(rest)
    case Nil => true
  }
  
  //lazy val id = quoteId | plainId
  lazy val varId = id filter { id => id.charAt(0) isLowerCase }
  lazy val rightOp = id filter { id => id.endsWith(":") }
    
  lazy val infixType : SyntaxRule[Type] = rightAssociativeInfixType | compoundType >> optInfixType
  lazy val rightAssociativeInfixType : SyntaxRule[Type] = compoundType ~ rightOp ~- (nl?) ~ (rightAssociativeInfixType | compoundType) ^~~^ InfixType
  def optInfixType(left : Type) : SyntaxRule[Type] = infixType(left) >> optInfixType | unit(left)
  def infixType(left : Type) = id ~- (nl?) ~ compoundType ^^ { case id ~ right => InfixType(left, id, right) }
      
  lazy val compoundType : SyntaxRule[Type] = (refinement 
      | annotType ~- !("with" | refinement) 
      | annotType ~ ("with" -~ annotType *) ~ (refinement?) ^~~^ CompoundType)
  lazy val refinement : SyntaxRule[Refinement] = (nl?) -~ curly((dcl | typeDef)*/semi) ^^ Refinement
  
  // TODO: report issue with AnnotType definition in syntax summary
  lazy val annotType = simpleType ~ (annotation+) ^~^ AnnotatedType | simpleType

  lazy val simpleType = (path ~- '.' ~- "type" ^^ SingletonType
      | stableId ^^ { list => { val Name(id) :: rest = list.reverse; TypeDesignator(rest.reverse, id) }}
      | round(types ~- (','?)) ^^ TupleType) >> typeArgsOrProjection
      
  def typeArgsOrProjection(simpleType : Type) : SyntaxRule[Type] = (
      (typeArgs ^^ ParameterizedType(simpleType)) >> typeArgsOrProjection
      | '#' -~ (id ^^ TypeProjection(simpleType)) >> typeArgsOrProjection
      | unit(simpleType))
      
  // TODO: Changed by me to allow '_' - resolve with spec
  lazy val typeArgs = square((typeSpec | '_' -^ TypeDesignator(Nil, "_"))+/',')
  lazy val types = typeSpec+/','

  lazy val expr : SyntaxRule[Expression] = ((bindings | untypedIdBinding) ~- `=>` ~ expr ^~^ FunctionExpression as "expr") | expr1

  // TODO : SLS definition for Typed Expression appears wrong.  Have raised ticket #263 - update when outcome known.
  lazy val expr1 : SyntaxRule[Expression] = (
      "if" -~ round(expr) ~- (nl*) ~ expr ~ ((semi?) -~ "else" -~ expr ?) ^~~^ IfExpression
      | "while" -~ round(expr)  ~- (nl*) ~ expr ^~^ WhileExpression
      | "try" -~ curly(block) ~ ("catch" -~ curly(caseClauses) ?) ~ ("finally" -~ expr ?) ^~~^ TryCatchFinally
      | "do" -~ expr ~- (semi?) ~- "while" ~ round(expr) ^~^ DoExpression
      | "for" -~ (round(enumerators) | curly(enumerators))  ~- (nl*) ~ (("yield"?) ^^ (_.isDefined)) ~ expr ^~~^ ForComprehension
      | "throw" -~ expr ^^ Throw
      | "return" -~ (expr?) ^^ Return
      | assignment
      | postfixExpr ~- ':' ~ typeSpec ^~^ TypedExpression
      | postfixExpr ~- ':' ~ (annotation+) ^~^ AnnotatedExpression
      | postfixExpr ~- ':' ~- '_' ~- '*' ^^ VarArgExpression
      | postfixExpr ~- "match" ~ curly(caseClauses) ^~^ MatchExpression as "expr1") | postfixExpr
      
  lazy val assignment = simpleExpr ~- '=' ~ expr ^^? {
    case Name(id) ~ value => SimpleAssignment(id, value)
    case DotExpression(expr, Name(id)) ~ value => DotAssignment(expr, id, value)
    case ApplyExpression(expr, args) ~ value => Update(expr, args, value)
  }
    
  lazy val postfixExpr = (infixExpr ~ id ^~^ PostfixExpression as "postfixExpr") | infixExpr
      
  /** InfixExpr ::= PrefixExpr | InfixExpr id [nl] InfixExpr */
  lazy val infixExpr = infix(operators)
  
  def infix(operators : List[SyntaxRule[(Expression, Expression) => Expression]]) : SyntaxRule[Expression] = {
    val op :: tail = operators
    val next = if (tail == Nil) prefixExpr else infix(tail)
    next ~*~ op
  }
  
  def infixId(choices : String) : SyntaxRule[String] = id filter { string => choices contains (string.charAt(0)) }
  def infixOp(rule : SyntaxRule[String]) : SyntaxRule[(Expression, Expression) => Expression] = rule ~- (nl?) ^^ { id => InfixExpression(id, _ : Expression, _ : Expression) }
      
  /** Infix operators in list from lowest to highest precedence */
  lazy val operators : List[SyntaxRule[(Expression, Expression) => Expression]] = List[SyntaxRule[String]](
      infixId("_$") | id filter(_.charAt(0).isLetter),
      infixId("|"),
      infixId("^"),
      infixId("&"),
      infixId("<>"),
      infixId("=!"),
      infixId(":"),
      infixId("+-"),
      infixId("*%/"),
      otherOp) map infixOp
      
  lazy val otherOp = id filter { string => 
        val first = string.charAt(0)
        !first.isLetter && !"_$|^&<>=!:+-*%/".contains(first)
  }

  lazy val prefixExpr = ("+" | "-" | "!" | "~") ~ simpleExpr ^~^ PrefixExpression | simpleExpr

  /**
  SimpleExpr ::= new (ClassTemplate | TemplateBody)
                      | BlockExpr
                      | SimpleExpr1 [_]
  SimpleExpr1 ::= Literal
                      | Path
                      | _
                      | ( [Exprs [,]] )
                      | SimpleExpr . id
                      | SimpleExpr TypeArgs
                      | SimpleExpr1 ArgumentExprs
                      | XmlExpr
  */

  lazy val simpleExpr = ("new" -~ classTemplate ^^ InstanceCreation
      | blockExpr
      | simpleExpr1) >> simpleExprRest

  lazy val simpleExpr1 : SyntaxRule[Expression] = ('_' -^ Underscore
      | literal
      | scanner.xmlExpr
      | pathElement
      | tupleExpr) >> simpleExpr1Rest
      
  def simpleExprRest(expr : Expression) : SyntaxRule[Expression] = (
      '.' -~ (pathElement ^^ (DotExpression(expr, _))) >> simpleExprRest
      | (typeArgs ^^ (ExpressionTypeArgs(expr, _))) >> simpleExprRest
      | simpleExpr1Rest(expr))
      
  def simpleExpr1Rest(expr : Expression) : SyntaxRule[Expression] = (
      '_' -^ Unapplied(expr) >> simpleExprRest
      | (argumentExprs ^^ (ApplyExpression(expr, _))) >> simpleExprRest
      | unit(expr))
      
  lazy val tupleExpr = round(exprs ~- (','?) | nil) ^^ TupleExpression
  lazy val exprs = expr +/','
  lazy val argumentExprs = round(exprs ~- (','?) | nil) | (nl?) -~ blockExpr ^^ (List(_))

  lazy val blockExpr : SyntaxRule[Expression] = curly(caseClauses | block) as "blockExpr"
  lazy val block : SyntaxRule[Block] = (blockStat ~- semi *) ~ (resultExpr?) ^~^ Block
  
  // TODO: This is different from what's in the spec.  Resolve
  lazy val blockStat  : SyntaxRule[Statement] = (importStat
      | (annotation*) ~ (localModifier*) ~ definition ^~~^ AnnotatedDefinition
      | expr1
      | unit(EmptyStatement)) as "blockStat"

  lazy val resultExpr = (bindings | singleIdBinding ) ~- `=>` ~ block ^~^ FunctionExpression | expr1
  lazy val bindings = round(binding*/',')
  lazy val binding = id ~ (':' -~ typeSpec ?) ^~^ Binding
  lazy val singleIdBinding = id ~ (':' -~ compoundType ?) ^~^ Binding ^^ { List(_) }
  lazy val untypedIdBinding = id ~ none ^~^ Binding ^^ { List(_) }
  
  // Note: added deprecated syntax
  lazy val enumerators = (generator | deprecatedGenerator) ~++ (semi -~ enumerator *)
  
  lazy val generator = pattern1 ~- "<-" ~ expr ~ (guard?) ^~~^ Generator
  lazy val guard = "if" -~ postfixExpr
  lazy val enumerator : SyntaxRule[Enumerator] = (generator 
      | guard ^^ Guard
      | ("val" -~ pattern1 ~- '=') ~ expr ^~^ ValEnumerator 
      | deprecatedEnumerator)
      
  lazy val deprecatedGenerator = "val" -~ pattern1 ~- "<-" ~ expr ~ none ^~~^ Generator
  lazy val deprecatedEnumerator : SyntaxRule[Enumerator] = (deprecatedGenerator 
      | (pattern1 ~- '=') ~ expr ^~^ ValEnumerator 
      | postfixExpr ^^ Guard)

  lazy val caseClauses = (caseClause+) ^^ CaseClauses
  lazy val caseClause = "case" -~ singleStatement(pattern ~ (guard?)) ~- `=>` ~ block ^~~^ CaseClause

  lazy val pattern = pattern1 ~*~ ('|' -^ OrPattern)
  
  // Note: Changed by me to infixType
  lazy val pattern1 = (varId ~- ':' ~ infixType ^~^ TypedVariablePattern
      | '_' -~ ':' -~ infixType ^^ TypePattern
      | pattern2)
  lazy val pattern2 = (varId ~- '@' ~ pattern3) ^~^ AtPattern | pattern3
  
   lazy val pattern3 : SyntaxRule[Expression] = infixPattern(operators) | prefixExpr
      
  def infixPattern(operators : List[SyntaxRule[(Expression, Expression) => Expression]]) : SyntaxRule[Expression] = {
    val op :: tail = operators
    val next = if (tail == Nil) simplePattern else infixPattern(tail)
    next ~*~ (op.-[S]('|'))
  }
 
  // Changed to allow constant expressions like "-1" - note prefixExpr must be a constant expression
  // See ticket #264
  lazy val simplePattern : SyntaxRule[Expression] = ('_' -^ Underscore
      | literal
      | scanner.xmlPattern
      | stableId ~ round(pattern*/',' ~- (','?)) ^^ { case a ~ b => StableIdPattern(a, Some(b), false) }
      | stableId ~ round((pattern ~- ',' *) ~- '_' ~- '*') ^^ { case a ~ b => StableIdPattern(a, Some(b), true) }
      | round(patterns ~- (','?)) ^^ TupleExpression
      | varId ~- !'.' ^^ VariablePattern
      | prefixExpr  // added by me, but prevents next alternative from succeeding
      | stableId  ^^ (StableIdPattern(_, None, false))) as "simplePattern"

  lazy val patterns = pattern+/','

  lazy val typeParamClause : SyntaxRule[List[VariantTypeParameter]] = square(variantTypeParam+/',')
  lazy val funTypeParamClause = square(typeParam+/',')

  lazy val variance = '+' -^ Covariant | '-' -^ Contravariant | unit(Invariant)
  lazy val variantTypeParam = variance ~ typeParam ^~^ VariantTypeParameter
  
  /** TypeParam ::= id [>: Type] [<: Type] [<% Type] */
  // TODO: Definition from SLS is wrong (no type parameters after id) - raise issue
  lazy val typeParam = (id | "_") ~ (typeParamClause?) ~ (">:" -~ typeSpec ?) ~ ("<:" -~ typeSpec ?) ~ ("<%" -~ typeSpec ?) ^~~~~^ TypeParameter

  lazy val paramClause = (nl?) -~ round(param*/',')
  lazy val param = (annotation*) ~ id ~ (paramType?) ^~~^ Parameter
  lazy val paramType = ':' -~ (`=>`-?) ~ typeSpec ~ ('*'-?) ^~~^ ParameterType

  lazy val modifier : SyntaxRule[Modifier] = localModifier | accessModifier | "override" -^ Override
  lazy val localModifier : SyntaxRule[Modifier]  = ("abstract" -^ Abstract
      | "final" -^ Final
      | "sealed" -^ Sealed
      | "implicit" -^ Implicit
      | "lazy" -^ Lazy)
  lazy val accessModifier : SyntaxRule[Modifier] = ("private" -~ (accessQualifier?) ^^ Private
      | "protected" -~ (accessQualifier?) ^^ Protected) 
  lazy val accessQualifier = square(id ^^ Name | "this" -^ This)

  lazy val annotation : SyntaxRule[Annotation] = '@' -~ annotationExpr ~- (nl?)
  lazy val annotationExpr = annotType ~ (argumentExprs*) ~ ((nl?) -~ curly(nameValuePair*/semi) | nil) ^~~^ Annotation
  lazy val nameValuePair = "val" -~ id ~- '=' ~ prefixExpr ^~^ Pair[String, Expression]

  lazy val optTemplateBody = templateBody ^^ Some[TemplateBody] | !((nl?) -~ '{') -^ None
  lazy val templateBody = (nl?) -~ curly(selfType ~ (templateStat*/semi)) ^~~^ TemplateBody

  lazy val selfType = ((id ^^ Some[String]) ~ (':' -~ typeSpec ?)  ~- `=>`
      | ("this" -^ None) ~ (':' -~ infixType ^^ Some[Type]) ~- `=>` 
      | none ~ none)
  
  lazy val templateStat = element(importStat
      | (annotation*) ~ (modifier*) ~ definition ^~~^ AnnotatedDefinition
      | (annotation*) ~ (modifier*) ~ dcl ^~~^ AnnotatedDeclaration
      | expr
      | unit(EmptyStatement))

  lazy val importStat : SyntaxRule[Statement] = "import" -~ (importExpr+/',') ^^ ImportStatement
  lazy val importExpr : SyntaxRule[Import] = (
      stableId ~- '.' ~ importSelectors
      | stableId ~- '.' ~ (wildcardImportSelector ^^ (List(_))) 
      | simpleImport) ^~^ Import
  lazy val simpleImport = path ^^ (_ reverse) ^^? {
    case Name(id) :: (rest @ Name(_) :: _) => rules.~(rest.reverse, List(ImportSelector(id, None)))
  }
  lazy val importSelectors = curly((importSelector ~- ',' *) ~ (importSelector | wildcardImportSelector)) ^^ { case ss ~ s => ss ::: s :: Nil }
  lazy val importSelector = id ~ (`=>` -~ (id | "_") ?) ^~^ ImportSelector
  lazy val wildcardImportSelector = '_' -^ ImportSelector("_", None)

  lazy val dcl = valDcl | varDcl | funDcl | typeDcl as "dcl"
  lazy val valDcl = "val" -~ ids ~- ':' ~ typeSpec ^~^ ValDeclaration
  lazy val varDcl = "var" -~ ids ~- ':' ~ typeSpec ^~^ VarDeclaration
  lazy val funDcl = "def" -~ funSig ~ (':' -~ typeSpec ?)  ^~~~~^ FunctionDeclaration
  lazy val typeDcl = "type" -~ (nl*) -~ id ~ (typeParamClause?) ~ (">:" -~ typeSpec ?) ~ ("<:" -~ typeSpec ?) ^~~~^ TypeDeclaration

  lazy val funSig = id ~ (funTypeParamClause?) ~ (paramClause*) ~ (implicitParamClause?)
  lazy val implicitParamClause = (nl?) -~ round("implicit" -~ (param+/','))
  
  lazy val definition : SyntaxRule[Definition] = (valPatDef
      | varPatDef
      | "var" -~ ids ~ (':' -~ typeSpec ~- '=' ~- '_') ^~^ VarDefaultDefinition
      | "def" -~ funSig ~ (':' -~ typeSpec ?) ~ ('=' -~ expr) ^~~~~~^ FunctionDefinition
      | "def" -~ funSig ~ ((nl?) -~ curly(block)) ^~~~~^ ProcedureDefinition
      | "def" -~ "this" -~ (paramClause+) ~ (implicitParamClause?) ~ ('=' -~ constrExpr | (nl?) -~ constrBlock) ^~~^ ConstructorDefinition
      | typeDef
      | tmplDef) as "definition"

  lazy val constrExpr = selfInvocation ^^ (ConstructorExpression(_, Nil)) | constrBlock
  lazy val constrBlock = curly(selfInvocation ~ (semi -~ blockStat *)) ^~^ ConstructorExpression
  lazy val selfInvocation = "this" -~ (argumentExprs+)

  lazy val valPatDef = "val" -~ patDef ^~~^ ValPatternDefinition
  lazy val varPatDef = "var" -~ patDef ^~~^ VarPatternDefinition
  lazy val patDef = (pattern2+/',') ~ (':' -~ typeSpec ?) ~ ('=' -~ expr)
  
  lazy val typeDef = "type" -~ (nl*) -~ id ~ (typeParamClause?) ~ ('=' -~ typeSpec) ^~~^ TypeDefinition
  lazy val tmplDef = classDef | objectDef | traitDef as "tmplDef"

  lazy val classDef = for {
    isCase <- ("case"-?)
    name <- "class" -~ id
    typeParams <- (typeParamClause?)
    annotations <- (annotation*)
    modifier <- (accessModifier?)
    params <- (classParamClause*)
    implicitParams <- (implicitClassParamClause?)
    template <- classTemplateOpt
  } yield ClassDefinition(isCase, name, typeParams, annotations, modifier, params, implicitParams, template)

  lazy val objectDef = ("case"-?) ~- "object" ~ id ~ classTemplateOpt ^~~^ ObjectDefinition

  lazy val traitDef = for {
    name <- "trait" -~ id
    typeParams <- (typeParamClause?)
    template <- traitTemplate
    val early ~ parents ~ body = template
  } yield TraitDefinition(name, typeParams, early, parents, body)

  lazy val classParamClause = (nl?) -~ round(classParam*/',')
  lazy val implicitClassParamClause = (nl?) -~ round("implicit" -~ (classParam+/','))
  lazy val classParam = (annotation*) ~ (classParamModifiers?) ~ id ~ (paramType ?) ^~~~^ ClassParameter

  lazy val classParamModifiers = ((modifier*) ~- "val" ^^ ValParameterModifiers 
      | (modifier*) ~- "var" ^^ VarParameterModifiers)

  lazy val classTemplateOpt = ("extends" -~ classTemplate
      | none ~ none ~ nil ~ nil ~ ("extends" -~ templateBody ^^ Some[TemplateBody]) ^~~~~^ ClassTemplate
      | none ~ none ~ nil ~ nil ~ optTemplateBody ^~~~~^ ClassTemplate)
      
  lazy val classTemplate = (earlyDefs?) ~ (annotType ^^ Some[Type]) ~ (argumentExprs*) ~ ("with" -~ annotType *) ~ optTemplateBody  ^~~~~^ ClassTemplate

  lazy val traitTemplate= ("extends" -~ (earlyDefs?) ~ (annotType +/ "with") ~ optTemplateBody
      | none ~ nil ~ ("extends" -~ templateBody ^^ (Some(_)))
      | none ~ nil ~ optTemplateBody)

  lazy val earlyDefs = curly(earlyDef+/semi) ~- "with"
  lazy val earlyDef = (annotation*) ~ (modifier*) ~ (valPatDef | varPatDef) ^~~^ AnnotatedDefinition

  lazy val topStatSeq = topStat*/semi
  lazy val topStat : SyntaxRule[Element[Statement]] = element(importStat
      | packaging
      | (annotation*) ~ (modifier*) ~ tmplDef ^~~^ AnnotatedDefinition)

  lazy val packaging = ("package" -~ qualId) ~ ((nl?) -~ curly(topStatSeq)) ^~^ Packaging
  lazy val compilationUnit = ("package" -~ qualId ~- semi ?) ~ topStatSeq ~- !item ^~^ CompilationUnit
}
