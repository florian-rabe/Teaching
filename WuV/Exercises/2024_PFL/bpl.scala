// 0) an abstract class for all non-terminals, covering everything in the syntax
//    * in particular, this has a print method
sealed abstract class SyntaxFragment {
   def print(): String
}

// 1) one type (represented as an abstract class) for every non-terminal

// P
// abstract class AbsProgram -- not needed because of the exception below
// D
sealed abstract class Declaration extends SyntaxFragment {
   def name: String
}
// E
sealed abstract class Expression extends SyntaxFragment

// 2) for every production N ::= t_0 N_0 ... N_i t_i
//    * one constructor (represented as a concrete class, in particular a case class in Scala)
//    * invent a name for the production that is used as the name of the class
//    * extends the class corresponding to the non-terminal on the LHS (i.e., N)
//    * one constructor argument for every non-terminal occurring on the RHS (i.e., N_0, ..., N_i)
//      * the non-terminal is the type of the constructor argument
//        * if the non-terminal occurs as N^*, we can use List[N]
//        * if the non-terminal occurs as [N], we can use Option[N]
//      * invent a name for every non-terminal occurrence on the RHS that is used as the name of the constructor argument
//    * a print method that outputs the concrete syntax, which in particular inserts the terminal parts (i.e., the t_0, ..., t_i)

/* Scala built-ins for options and lists that we can assume:

abstract class Option[T]
case class None[T] extends Option[T]
case class Some[T](value: T) extends Option[T]

abstract class List[T] {
   def map ...
   def mkString ...
}
case class Nil[T] extends List[T]
case class Cons[T](head: T, tail: List[T]) extends List[T]
*/

// Exception: If there is only one production for a non-terminal (and there never will be another one),
// we can skip the abstract class form step (1)

// P ::= D^* E
// e.g.
// def_1
// ...
// def_n
// main
case class Program(defs: List[Declaration], main: Expression) extends SyntaxFragment {
   def print(): String = {
     defs.map(_.print()+"\n").mkString("\n") + "\n" + main.print()
   }
}

case class VarDecl(name: String, tp: Type) extends SyntaxFragment {
  def print(): String = name + " : " + tp.print()
}

// D ::= def I((x:Y)*): E^*
// e.g.
// def name(x:int, y: bool, ...) = body
case class FunDef(name: String, inputs: List[VarDecl], outputType: Type, body: Expression) extends Declaration {
   def print(): String = {
      "def " + name + "(" + inputs.mkString(",") + "): " + outputType.print() + " = " + body.print()
   }
}

/*
case class ImportDecl(package: String) extends Declaration {
  def print() : String = {"import " + package}
}*/

// E ::= {E^*}
case class Block(exprs: List[Expression]) extends Expression {
  def print(): String = {
     "{" + exprs.map(_.print()).mkString("; ") + "}"
  }
}

// E ::= return E
case class Return(r: Expression) extends Expression {
   def print() = "return " + r.print()
}

// E ::= f(E^*)
// e.g.
// f(arg_1, ..., arg_n)
case class FunApply(name: String, args: List[Expression]) extends Expression {
   def print() = name + "(" + args.map(_.print()).mkString(",") + ")"
}

// E ::= E O E
// e.g.
// left operator right
case class InfixOperatorApply(operator: String, left: Expression, right: Expression) extends Expression {
   // prints all brackets even if redundant, e.g., (x + (2*y))
   def print() = "(" + left.print() + " " + operator + " " + right.print() + ")"
}

// E :: var x: Y = E
// declaration of a variable
case class VarDef(vr: VarDecl, initValue: Expression) extends Expression {
  def print() = "var " + vr.print() + " = " + initValue.print()
}

// E ::= x
// reference to variable called 'name'
case class Var(name: String) extends Expression {
   def print() = name
}

// E ::= x = E
// assignment to a variable
case class VarAssign(name: String, newValue: Expression) extends Expression {
   def print() = name + " = " + newValue.print()
}

/* removed - special case of FunApply
// E ::= print(E)
case class Print(e: Expression) extends Expression {
   def print() = "print(" + e.print() + ")"
}

// E ::= read(E)
case class Read(e: Expression) extends Expression {
   def print() = "read(" + e.print() + ")"
}
*/

// E ::= if (E) E [else E]
case class IfThenElse(condition: Expression, thenBranch: Expression, elseBranch: Option[Expression]) extends Expression {
   def print(): String = {
      val elsePrinted = elseBranch match {
        case None => ""
        case Some(e) => " else " + e.print()
      }
      "if (" + condition.print() + ")" + thenBranch.print() + elsePrinted
   }
}

// E ::= while (E) {E}
case class While(condition: Expression, body: Expression) extends Expression {
  def print(): String = {
     "while (" + condition.print() + ") " + body.print()
  }
}

// E ::= for I in E {E}
case class For(vr: String, range: Expression, body: Expression) extends Expression {
   def print(): String = {
       "for " + vr + " in " + range.print() + " " + body.print()
   }
}

// E ::= N
// integer literal of value 'value'
case class IntegerLiteral(value: Int) extends Expression {
   def print() = value.toString()
}

// E ::= digits . digits
// float literal of value 'value'
case class FloatLiteral(value: Float) extends Expression {
   def print() = value.toString()
}

// E ::= true | false
case class BoolLiteral(value: Boolean) extends Expression {
   def print() = value.toString()
}

// E ::= string
// string literal of value 'value'
case class StringLiteral(value: String) extends Expression {
   def print() = "\"" + value.toString() + "\""
}

// E ::= ' c '
// charact literal of value 'value'
case class CharLiteral(value: Character) extends Expression {
   def print() = "'" + value.toString() + "'"
}

case class UnitExpr() extends Expression {
   def print () = "()"
}

/*
  General intuitions for types
  * Typically one type added to every identifier declaration.
  * Standard notation "id: type" (or in some languages: "type id")
  * 2 kinds of type systems: extrinsic vs. intrinsic; here: intrinsic
    ---> we can infer a unique type for every expressions
         types do not overlap
  * Choice of types up to language designer
    typically: at least built-in base type like int, bool
    often: builtin-type constructors like list, option, product, ...
    possibly: declarations that introduce types like classes
  * Built-in type: names is introduced in the grammar; user-defined type: name is introduced in the vocabulary
  * In the grammar, types comes in triplets of
    - type production that creates the type
      eg: Y ::= string                                  Y ::= (Y,Y)
    - expression productions for the constructors (= how to create values of the type)
      eg: Y ::= " CHARACTER* " (string literals)        E ::= (E,E)
    - expression production for the accessors (= what to do with values of the type)
      intuition: all accessors together allow recovering the specific value
      eg: E ::= length(E) | charAt(E,E)                 E ::= E.1 | E.2
*/

abstract class Type extends SyntaxFragment

// Y ::= int
// constructors: integer literals
// accessors: binary operators like +, *, ...; for-loops
case class IntegerType() extends Type {
  def print() = "int"
}

// Y ::= char
// constructors: one constructor for each charactor, 'a', '1', ...
// accessors: the unicode number of the character, charToString
case class CharType() extends Type {
   def print() = "char"
}

// Y ::= string
// constructors: string literals
// accessors: length, charAt
case class StringType() extends Type {
  def print() = "string"
}

// Y ::= float
// constructors: float literals
// accessors: binary operators like +, *, ...
case class FloatType() extends Type {
  def print() = "float"
}

// Y ::= bool
// constructors: true/false
// accessors: if-then-else
case class BoolType() extends Type {
  def print() = "bool"
}

// Y ::= unit
// special type containing only one value, written ()
case class UnitType() extends Type {
  def print() = "unit"
}

// Y ::= empty
// special type containing no values
case class EmptyType() extends Type {
  def print() = "empty"
}

// Y ::= list[Y]
// lists containing elements of type Y
case class ListType(elemType: Type) extends Type {
  def print() = "list[" + elemType + "]"
}


/* parsing is dual to printing, converting strings to syntax fragments
   Essentially, one method of type string -> N per non-terminal N
   Many different approaches for parsers exist because parsing is so difficult.
   The easiest style (hopefully) is the following:
     * keep the input string to parse as a variable
     * keep a variable index telling us where in the input we are
     * each method parseX
       * consumes characters at the position 'index' of the input
       * advances the index accordingly
       * and returns an X
*/
class Parser(input: String) {
  private var index = 0

  case class ParseError(msg: String) extends Error(msg)
  def fail(msg: String) = {
     throw ParseError(msg + "; found " + input.substring(index))
  }

  // all input has been parsed
  def atEnd = {index == input.length}

  // string s occurs at the current index in the input
  def startsWith(s: String): Boolean = {
     index+s.length <= input.length &&
     input.substring(index, index+s.length) == s
  }

  def next = input(index)

  // parses all whitespace at the current position
  def trim = {while (!atEnd && next.isWhitespace) index += 1}

  // parses the string s and throws it away
  def skip(s: String) = {
    if (!startsWith(s)) fail("expected " + s)
    index += s.length
  }

  def parseName = {
    val begin = index
    while (!atEnd && next.isLetterOrDigit) index += 1
    input.substring(begin,index)
  }
  
  def parseProgram: Program = {
    var decls: List[Declaration] = Nil
    trim
    while (startsWith("def")) {
       decls = decls ++ List(parseDeclaration)
       trim
    }
    val main = parseExpression
    trim
    if (!atEnd) fail("input left after parsing")
    Program(decls,main)
  }
  
  def parseDeclaration: Declaration = {
    if (startsWith("def")) {
        skip("def")
        trim
        val n = parseName
        trim
        skip("(")
        var inputs: List[VarDecl] = Nil
        while (next != ')') {
          inputs = inputs ++ List(parseVarDecl)
          trim
          if (next != ')') skip(",")
          trim
        }
        skip(")")
        trim
        skip(":")
        val outTp = parseType
        trim
        skip("=")
        val body = parseExpression
        FunDef(n, inputs, outTp, body)
    } else {
        throw ParseError("keyword expected")
    }
  }

  def parseVarDecl: VarDecl = {
    val n = parseName
    trim
    skip(":")
    val t = parseType
    VarDecl(n,t)
  }
  
  def parseExpression: Expression = {
     trim
     if (startsWith("{")) {
        skip("{")
        var es: List[Expression] = Nil
        while (next != '}') {
           es = es ++ List(parseExpression)
           trim
           if (next != '}') {
             skip(";")
             trim
           }
        }
        skip("}")
        Block(es)
     } else if (startsWith("var")) {
        skip("var")
        trim
        val vd = parseVarDecl
        trim
        skip("=")
        trim
        val iv = parseExpression
        VarDef(vd, iv)
     } else if (startsWith("return")) {
        skip("return")
        val e = parseExpression
        Return(e)
     } else if (startsWith("while")) {
        skip("while")
        val c = parseExpressionInBrackets
        val b = parseExpression
        While(c,b)
     } else if (startsWith("for")) {
        skip("for")
        val v = parseName
        trim
        skip("in")
        trim
        val r = parseExpression
        val b = parseExpression
        For(v,r,b)
     } else if (startsWith("if")) {
        skip("if")
        val c = parseExpressionInBrackets
        val th = parseExpression
        trim
        val el = if (startsWith("else")) {
           skip("else")
           Some(parseExpression)
        } else None
        IfThenElse(c,th,el)
     } else if (startsWith("\"")) {
        skip("\"")
        val begin = index
        while (next != '"') index += 1
        val end = index
        skip("\"")
        StringLiteral(input.substring(begin,end))
     } else if (startsWith("'")) {
        skip("'")
        val c = next
        skip(c.toString)
        skip("'")
        CharLiteral(c)
     } else if (startsWith("true")) {
        skip("true")
        BoolLiteral(true)
     } else if (startsWith("false")) {
        skip("false")
        BoolLiteral(false)
     } else if (startsWith("()")) {
        skip("()")
        UnitExpr()
     } else if(next.isDigit || next == '-') {
        val begin = index
        if (next == '-') skip("-")
        var seenDot = false
        while (!atEnd && (next.isDigit || (!seenDot && next == '.'))) {
          if (next == '.') seenDot = true
          index+=1
        }
        val s = input.substring(begin,index)
        if (seenDot) FloatLiteral(s.toFloat) else IntegerLiteral(s.toInt)
     } else if (next == '(') {
        skip("(")
        val l = parseExpression
        trim
        val o = next.toString
        skip(o)
        trim
        val r = parseExpression
        trim
        skip(")")
        InfixOperatorApply(o, l, r)
     } else {
        // function application, variable assignment, or variable reference
        val n = parseName
        trim
        if (next == '(') {
          skip("(")
          var es : List[Expression] = Nil
          while (next != ')') {
            es = es ++ List(parseExpression)
            trim
            if (next != ')') {
              skip(",")
              trim
            }
          }
          skip(")")
          FunApply(n, es)
        } else if (next == '=') {
          skip("=")
          trim
          val nv = parseExpression
          VarAssign(n, nv)
        } else {
          Var(n)
        }
     }
  }

  def parseExpressionInBrackets = {
     trim
     skip("(")
     val e = parseExpression
     trim
     skip(")")
     e
  }
  
  def parseType: Type = {
    trim
         if (startsWith("int")) {skip("int"); IntegerType()}
    else if (startsWith("float")) {skip("float"); FloatType()}
    else if (startsWith("char")) {skip("char"); CharType()}
    else if (startsWith("string")) {skip("string"); StringType()}
    else if (startsWith("bool")) {skip("bool"); BoolType()}
    else if (startsWith("unit")) {skip("unit"); UnitType()}
    else if (startsWith("empty")) {skip("empty"); EmptyType()}
    else if (startsWith("list[")) {
      skip("list[")
      val y = parseType
      skip("]")
      ListType(y)
    }
    else fail("unknown type")
  }
  
}

// alternative printer: all print action collected in one place
// not complete; not used anymore
object Printer {
   def print(sf: SyntaxFragment): String = {
     sf match {
       case Program(defs,mn) =>
         defs.map(print(_)+"\n").mkString("\n") + "\n" + print(mn)
       case FunDef(name, ins, out, body) => 
         "def " + name + "(" + ins.map(print(_)).mkString(",") + "): " + print(out) + " = "  + print(body)
       case VarDecl(n,y) => n + ": " + print(y)
       case Block(es) =>
         "{" + es.map(print(_)).mkString("; ") + "}"
       case Return(r) =>
         "return " + print(r)
       case FunApply(n,as) =>
         n + "(" + as.map(print(_)).mkString(",") + ")"
       case InfixOperatorApply(o,l,r) =>
         "(" + print(l) + " " + o + " " + print(r) + ")"
       case VarDef(vd,iv) => "var " + print(vd) + " = " + print(iv)
       case Var(n) => n
       case VarAssign(n,nv) => n + " = " + print(nv)
       case IfThenElse(c,th,el) =>
         val elsePrinted = el match {
          case None => ""
          case Some(e) => " else " + print(e)
         }
         "if (" + print(c) + ")" + print(th) + elsePrinted
       case While(c, b) =>
         "while (" + print(c) + ") " + print(b)
       case For(v,r,b) =>
         "for " + v + " in " + print(r) + " " + print(b)
       case IntegerLiteral(v) => v.toString()
       case StringLiteral(v) => "\"" + v + "\""
       case BoolLiteral(v) => v.toString()
       case FloatLiteral(v) => v.toString()
       case CharLiteral(v) => "'" + v.toString() + "'"
       
       case IntegerType() => "int"
       case BoolType() => "bool"
       case StringType() => "string"
       case FloatType() => "float"
       case CharType() => "char"
     }      
   }
}

/* There are two kinds of names:
   - global declarations: visible everywhere, usable from the outside (e.g., when importing a package), names can't be changed easily
     e.g., def f(...): ... = ..., also classes, types, ...
   - local (variable) declarations: visible in the current block only, names can be changed easily 
     e.g., variable declarations x:Y
*/
   
// the vocabulary tracks the list of declarations that are in scope
case class Vocabulary(decls: List[Declaration]) {
   def lookup(n: String) : Option[Declaration] = decls.find(_.name == n)
   def append(d: Declaration) = Vocabulary(decls ++ List(d))
}

// the context tracks the list of variable declarations that are in scope
case class Context(vardecls: List[VarDecl]) {
   def lookup(n: String) : Option[VarDecl] = vardecls.find(_.name == n)
   def append(vd: VarDecl) = Context(vardecls ++ List(vd))  
}

// a second AST traversal: checking
// often: it is more succinct to have a separate function for each non-terminal; the Printer and PythonTranslator could be adjusted accordingly
object Checker {
   case class SyntaxError(msg: String) extends Error(msg)
  
   val infixOperators = List("+", "*", "-", "&&", "||", "/", "<", ">", "++", "==", "!=")
   val builtInFunctions = List("print", "read", "stringToInt", "stringToFloat", "intToFloat", "charToString", "length", "charAt")

   val forbiddenNames = List("def", "for", "in", "while", "if", "else", "print", "return", "var")
   def checkName(n: String) = !forbiddenNames.contains(n) && n != "" && n(0).isLetter && n.forall(_.isLetterOrDigit)
   
   // if we ever need to check an arbitrary syntax fragment, this method delegates to the proper specialized method
   // but normally, we know what kind of fragment we have and call the specialized method directly
   def check(voc: Vocabulary, sf: SyntaxFragment): Unit = {
     sf match {
       case p: Program => checkProgram(voc, p)
       case d: Declaration => checkDeclaration(voc, d)
       case v: VarDecl => checkVarDecl(voc, v)
       case e: Expression => inferExpression(voc, Context(Nil), e, None)
       case y: Type => checkType(voc, y)
     }
   }
       
   // now one check method for each non-terminal N
   // every check method takes a vocabulary and an N-object and raises errors if it finds any
   def checkProgram(voc: Vocabulary, p: Program) = p match {
       case Program(defs,mn) =>
         var vocE = voc
         // check each declaration relative to the preceding ones (vocE)
         defs.foreach {d =>
            checkDeclaration(vocE, d)
            vocE = vocE.append(d)
         }
         // check the main expression relative to the whole vocabulary
         // at this point, there are no variables declared, so we use the empty context
         // no return statement is allowed, so we do not pass a returnType
         inferExpression(vocE, Context(Nil), mn, None)
   }
   
   def checkDeclaration(voc: Vocabulary, d: Declaration) = d match {
       case FunDef(name, ins, out, body) =>
         // check the name and check that it's not defined yet
         checkName(name)
         voc.lookup(name) match {
           case Some(_) => throw SyntaxError("name " + name + " already defined")
           case None =>
         }
         // check that all variable names are different
         val varnames = ins.map(_.name)
         if (varnames.distinct.length != varnames.length) throw SyntaxError("variable names not unique")
         // check the input declarations
         ins.foreach(checkVarDecl(voc, _))
         // check the output type
         checkType(voc, out)
         // check the body relative to the input variables, against the output type, remembering the return type
         checkExpression(voc, Context(ins), body, out, Some(out))
   }
   
   def checkVarDecl(voc: Vocabulary, vd: VarDecl) = vd match {
     case VarDecl(n, y) =>
        checkName(n)
        checkType(voc, y)
   }
   
   
   /* Expressions are typed by the types. We write e:y if expression e has type y.
      Checking of expressions must not only check that e is well-formed but also check e:y.
      It is typically done by two methods:
      * type-checking:  take e and y as input, check that e:y
      * type-inference: take e as input, compute and return y
      
      In an intrinsic type system, the intelligence can be mostly done by type inference,
      with type checking just comparing infered and expected type.
   */
   
   
   // type-check an expression against an expected type and an optional return type
   /* for example, when checking 
      def f(x:int): int = {var x: string = read("enter an int"); stringToInt(x)}
      the check for read("enter an int"), must look as follows: 
      checkExpression(voc, ctx, FunApply("read", List(StringLiteral("enter an int"))), StringType(), Some(IntType()))
   */
   def checkExpression(voc: Vocabulary, ctx: Context, e: Expression, expectedType: Type, returnType: Option[Type]): Unit = {
     // infer the type
     val inferedType = inferExpression(voc, ctx, e, returnType)
     // check it is equal to the expected one
     if (inferedType != expectedType) {
       throw SyntaxError("error while checking " + e.print() + ": " + "expected " + expectedType.print() + "; found " + inferedType.print())
     }
   }
   
   // infer the type of an expression
   def inferExpression(voc: Vocabulary, ctx: Context, exp: Expression, returnType: Option[Type]): Type = exp match {
       case Block(es) =>
         var currentContext = ctx
         var inferredType : Type = null
         es.foreach {e =>
           inferredType = inferExpression(voc, currentContext, e, returnType)
           e match {
             case VarDef(vd, _) => currentContext = currentContext.append(vd)
             case _ => 
           }
         }
         if (inferredType == null) {
           // block was empty
           UnitType()
         } else {
           // type of block is the type of the last expression in the block
           inferredType
         }
       case Return(r) =>
         returnType match {
           case None => throw SyntaxError("return statement not allowed here")
           case Some(y) =>
             checkExpression(voc, ctx, r, y, returnType)
             EmptyType()
         }
       case FunApply(n,as) =>
         if (builtInFunctions.contains(n)) {
           n match {
             case "print" => 
               if (as.length != 1) throw SyntaxError("one argument expected")
               val a = as(0)
               val aI = inferExpression(voc, ctx, a, returnType)
               UnitType()
             case "read" => 
               if (as.length != 1) throw SyntaxError("one argument expected")
               val a = as(0)
               checkExpression(voc, ctx, a, StringType(), returnType)
               StringType()
             case "stringToInt" => 
               if (as.length != 1) throw SyntaxError("one argument expected")
               val a = as(0)
               checkExpression(voc, ctx, a, StringType(), returnType)
               IntegerType()
             case "stringToFloat" => 
               if (as.length != 1) throw SyntaxError("one argument expected")
               val a = as(0)
               checkExpression(voc, ctx, a, StringType(), returnType)
               FloatType()
             case "charToString" => 
               if (as.length != 1) throw SyntaxError("one argument expected")
               val a = as(0)
               checkExpression(voc, ctx, a, CharType(), returnType)
               StringType()
             case "length" => 
               if (as.length != 1) throw SyntaxError("one argument expected")
               val a = as(0)
               checkExpression(voc, ctx, a, StringType(), returnType)
               IntegerType()
             case "charAt" => 
               if (as.length != 2) throw SyntaxError("two arguments expected")
               val a = as(0)
               val b = as(1)
               checkExpression(voc, ctx, a, StringType(), returnType)
               checkExpression(voc, ctx, b, IntegerType(), returnType)
               CharType()
           }
         } else {
           voc.lookup(n) match {
            case None => throw SyntaxError("name " + n + " not declared")
            case Some(FunDef(_, ins, out, bd)) => 
              if (ins.length != as.length)
                throw SyntaxError("function " + n + " applied to wrong number of arguments" +
                     " (expected: " + ins.length + "; found: " + as.length + ")")
              // (as zip ins) is the list of pairs of argument expressions and input variable declarations
              // the foreach-loop checks each argument a against the type of the corresponding variable declaration in
              (as zip ins).foreach {case (a,in) => checkExpression(voc, ctx, a, in.tp, returnType)}
              out
            case Some(_) => throw SyntaxError("name " + n + " exists but does not refer to a function")
           }
         }
       case InfixOperatorApply(o,l,r) =>
         if (!infixOperators.contains(o)) throw SyntaxError("unknown operator")
         o match {
           case "==" | "!=" =>
             val y = inferExpression(voc, ctx, l, returnType)
             checkExpression(voc, ctx, r, y, returnType)
             BoolType()
           case "+" | "*" | "-" | "/" =>
             val lI = inferExpression(voc, ctx, l, returnType)
             val rI = inferExpression(voc, ctx, r, returnType)
             (lI, rI) match {
               case (IntegerType(), IntegerType()) =>
                 if (o == "/") FloatType() else IntegerType()
               case (FloatType(), FloatType()) => FloatType()
               case (IntegerType(), FloatType()) => FloatType()
               case (FloatType(), IntegerType()) => FloatType()
               case _ => throw SyntaxError("operator used with bad type: " + o + "(" + lI.print() + ", " + rI.print() + ")")
             }
           case "&&" | "||" =>
              checkExpression(voc, ctx, l, BoolType(), returnType)
              checkExpression(voc, ctx, r, BoolType(), returnType)
              BoolType()
           case "<" | ">" =>
             val lI = inferExpression(voc, ctx, l, returnType)
             val rI = inferExpression(voc, ctx, r, returnType)
             (lI, rI) match {
               case (IntegerType(), IntegerType()) =>
               case (FloatType(), FloatType()) =>
               case (IntegerType(), FloatType()) =>
               case (FloatType(), IntegerType()) =>
               case _ => throw SyntaxError("operator used with bad type: " + o + "(" + lI.print() + ", " + rI.print() + ")")
             }
             BoolType()
           case "++" =>
             checkExpression(voc, ctx, l, StringType(), returnType)
             checkExpression(voc, ctx, r, StringType(), returnType)
             StringType()
         }
       case VarDef(vd,iv) =>
         checkVarDecl(voc, vd)
         checkExpression(voc, ctx, iv, vd.tp, returnType)
         UnitType()
       case Var(n) =>
          // look up n in context
          ctx.lookup(n) match {
            case None => throw SyntaxError("undeclared variable")
            case Some(VarDecl(_, y)) => y
          }
       case VarAssign(n,nv) =>
          ctx.lookup(n) match {
            case None => throw SyntaxError("undeclared variable")
            case Some(VarDecl(_, y)) => checkExpression(voc, ctx, nv, y, returnType)
          }
          UnitType()
       case IntegerLiteral(v) => IntegerType()
       case StringLiteral(v) => StringType()
       case CharLiteral(v) => CharType()
       case FloatLiteral(v) => FloatType()
       case BoolLiteral(v) => BoolType()
       case UnitExpr() => UnitType()
       case IfThenElse(c,th,el) =>
         checkExpression(voc, ctx, c, BoolType(), returnType)
         val thInferred = inferExpression(voc, ctx, th, returnType)
         el match {
          case None =>
            UnitType()
          case Some(e) =>
            checkExpression(voc, ctx, e, thInferred, returnType)
            thInferred
         }
       case While(c, b) =>
         checkExpression(voc, ctx, c, BoolType(), returnType)
         inferExpression(voc, ctx, b, returnType)
         UnitType()
       case For(v,l,b) =>
         checkName(v)
         inferExpression(voc, ctx, l, returnType) match {
           case ListType(y) =>
             inferExpression(voc, ctx.append(VarDecl(v,y)), b, returnType)
             UnitType()
           case _ => throw SyntaxError("for-loop must run over a list")
         }
     }

     def checkType(voc: Vocabulary, y: Type): Unit = y match {
       // nothing to check for the built-in base types
       case IntegerType() =>
       case BoolType() =>
       case FloatType() =>
       case CharType() =>
       case StringType() =>
       case UnitType() =>
       case EmptyType() =>
       case ListType(y) => checkType(voc, y)
     }
}

// Relative semantics: translate the program to another language, whose semantics is already defined
object PythonTranslator {
   def print(sf: SyntaxFragment): String = {
     sf match {
       case Program(defs,mn) =>
         defs.map(print(_)+"\n").mkString("\n") + "\n" + print(mn)
       case FunDef(name, ins, out, body) => 
         "def " + name + "(" + ins.map(print(_)).mkString(",") + "):\n" + print(body).indent(2)
       case VarDecl(n,y) => n
       case Block(es) =>
         es.map(print(_)).mkString("\n")
       case Return(r) =>
         "return " + print(r)
       case FunApply(n,as) =>
         n match {
           case "read" =>  "input(" + print(as(0)) + ")"
           case "print" | "int" | _ => n + "(" + as.map(print(_)).mkString(",") + ")"
         }
       case InfixOperatorApply(o,l,r) =>
         "(" + print(l) + " " + o + " " + print(r) + ")"
       case VarDef(vd, iv) => print(vd) + " = " + print(iv)
       case Var(n) => n
       case VarAssign(n,nv) => n + " = " + print(nv)
       case IntegerLiteral(v) => v.toString()
       case StringLiteral(s) => "\"" + s + "\""
       case IfThenElse(c,th,el) =>
         val elsePrinted = el match {
          case None => ""
          case Some(e) => "else:\n" + print(e).indent(2)
         }
         "if (" + print(c) + "):\n" + print(th).indent(2) + elsePrinted
       case While(c, b) =>
         "while (" + print(c) + "):\n" + print(b).indent(2)
       case For(v,r,b) =>
         "for " + v + " in " + print(r) + ":\n" + print(b).indent(2)
     }      
   }
}


object Main {
   def main(args: Array[String]) = {
      // val inFile = new os.Path(args(0))
      // val s = os.read(inFile)
      
      // an example program
      val progString =
"""
def sumFromTo(x: int, y: int): int  = if ((x > y))
    0
  else {
    var n: int = x;
    var s: int = 0;
    while ((n<y)) {
      s = (s+n);
      n = (n+1)
    };
    s
  }
def average(x: int, y: int): float = (sumFromTo(x,y)/(y-x))
def test(): int = {
  var x: int = stringToInt(read("Enter first number"));
  var y: int = stringToInt(read("Enter second number"));
  sumFromTo(x,y)
}
print(test())
"""   
      // Step 1 (context-free syntax): parse
      val prog = new Parser(progString).parseProgram
      
      // alternatively, we manually construct a program like
      /*
      val prog = Program(List(
        FunDef("f", List(VarDecl("x", IntegerType()), VarDecl("x", IntegerType())), IntegerType(),
           Return(
              InfixOperatorApply("+", Var("x"), InfixOperatorApply("*", IntegerLiteral(2), Var("y")))
           )
        ),
        FunDef("g", List(VarDecl("x", IntegerType())), IntegerType(),
           Return(
              FunApply("f", List(Var("x"), IntegerLiteral(1)))
           )
        )
      ),
        Print(FunApply("f", List(IntegerLiteral(1), IntegerLiteral(2)))
      )) */


      // Step 2 (context-sensitive syntax): check
      Checker.check(Vocabulary(Nil), prog)

      // optionally: print
      //println(prog.print())
      //println(Printer.print(prog))

      // Step 3 (semantics): translate to Python
      println(PythonTranslator.print(prog))
   }
}