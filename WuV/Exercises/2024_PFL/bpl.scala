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

// D ::= def I(I^*): E^*
// e.g.
// def name(varnames) = body
case class FunDef(name: String, varnames: List[String], body: Expression) extends Declaration {
   def print(): String = {
      "def " + name + "(" + varnames.mkString(",") + ") = " + body.print()
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

// E :: var I = E
// declaration of a variable
case class VarDecl(name: String, initValue: Expression) extends Expression {
  def print() = "var " + name + " = " + initValue.print()
}

// E ::= I
// reference to variable called 'name'
case class Var(name: String) extends Expression {
   def print() = name
}

// E ::= I = E
// assignment to a variable
case class VarAssign(name: String, newValue: Expression) extends Expression {
   def print() = name + " = " + newValue.print()
}

// E ::= N
// integer literal of value 'value'
case class IntegerLiteral(value: Int) extends Expression {
   def print() = value.toString()
}

// E ::= string
// string literal of value 'value'
case class StringLiteral(value: String) extends Expression {
   def print() = "\"" + value.toString() + "\""
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
        var varnames: List[String] = Nil
        while (next != ')') {
          varnames = varnames ++ List(parseName)
          trim
          if (next != ')') skip(",")
          trim
        }
        skip(")")
        trim
        skip("=")
        val body = parseExpression
        FunDef(n, varnames, body)
    } else {
        throw ParseError("keyword expected")
    }
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
        val n = parseName
        trim
        skip("=")
        trim
        val iv = parseExpression
        VarDecl(n, iv)
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
     } else if(next.isDigit || next == '-') {
        val begin = index
        if (next == '-') skip("-")
        while (!atEnd && next.isDigit) index+=1
        val i = input.substring(begin,index).toInt
        IntegerLiteral(i)
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
}


// alternative printer: all print action collected in one place
object Printer {
   def print(sf: SyntaxFragment): String = {
     sf match {
       case Program(defs,mn) =>
         defs.map(print(_)+"\n").mkString("\n") + "\n" + print(mn)
       case FunDef(name, varnames, body) => 
         "def " + name + "(" + varnames.mkString(",") + ") = "  + print(body)
       case Block(es) =>
         "{" + es.map(print(_)).mkString("; ") + "}"
       case Return(r) =>
         "return " + print(r)
       case FunApply(n,as) =>
         n + "(" + as.map(print(_)).mkString(",") + ")"
       case InfixOperatorApply(o,l,r) =>
         "(" + print(l) + " " + o + " " + print(r) + ")"
       case VarDecl(n,iv) => "var " + n + " = " + print(iv)
       case Var(n) => n
       case VarAssign(n,nv) => n + " = " + print(nv)
       case IntegerLiteral(v) => v.toString()
       case StringLiteral(v) => "\"" + v + "\n"
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
     }      
   }
}

case class Vocabulary(decls: List[Declaration]) {
   def lookup(n: String) : Option[Declaration] = decls.find(_.name == n)
   def append(d: Declaration) = Vocabulary(decls ++ List(d))
}

// a second AST traversal: checking
object Checker {
   case class SyntaxError(msg: String) extends Error(msg)
  
   val infixOperators = List("+", "*", "-", "&&", "||", "/", "<", ">")
   val builtInFunctions = List("print", "read", "int")

   val forbiddenNames = List("def", "for", "in", "while", "if", "else", "print", "return", "var")
   def checkName(n: String) = !forbiddenNames.contains(n) && n != "" && n(0).isLetter && n.forall(_.isLetterOrDigit)
   
   def check(voc: Vocabulary, sf: SyntaxFragment): Unit = {
     sf match {
       case Program(defs,mn) =>
         var vocE = voc
         defs.foreach {d =>
            check(vocE, d)
            vocE = vocE.append(d)
         }
         check(vocE, mn)
       case FunDef(name, varnames, body) => 
         checkName(name)
         voc.lookup(name) match {
           case Some(_) => throw SyntaxError("name " + name + " already defined")
           case None =>
         }
         if (varnames.distinct.length != varnames.length) throw SyntaxError("variable names not unique")
         varnames.foreach(checkName(_))
         check(voc, body)
       case Block(es) =>
         es.foreach(check(voc, _))
       case Return(r) =>
         check(voc, r)
       case FunApply(n,as) =>
         if (builtInFunctions.contains(n)) {
           // TODO: change this if there is ever a built-in function that expects more than 1 argument
           if (as.length != 1) throw SyntaxError("one arguments expected")
         } else {
           voc.lookup(n) match {
            case None => throw SyntaxError("name " + n + " not declared")
            case Some(FunDef(_, vrs, bd)) => 
              if (vrs.length != as.length)
                throw SyntaxError("function " + n + " applied to wrong number of arguments" +
                     " (expected: " + vrs.length + "; found: " + as.length + ")")
              as.foreach(check(voc, _))
            case Some(_) => throw SyntaxError("name " + n + " exists but does not refer to a function")
           }
         }
       case InfixOperatorApply(o,l,r) =>
         if (!infixOperators.contains(o)) throw SyntaxError("unknown operator")
         check(voc, l)
         check(voc, r)
       case VarDecl(n,iv) =>
         checkName(n)
         check(voc, iv)
       case Var(n) =>
         // TODO: where applicable, throw SyntaxError("variable " + n + " not in scope")
       case VarAssign(n,nv) =>
         // TODO: where applicable, throw SyntaxError("variable " + n + " not in scope")
         // TODO: check if n is mutable (i.e., if we're allowed to change the value of this variable)
         check(voc, nv)
       case IntegerLiteral(v) =>
       case StringLiteral(v) =>
       case IfThenElse(c,th,el) =>
         val elsePrinted = el match {
          case None =>
          case Some(e) => check(voc, e)
         }
         check(voc, c)
         check(voc, th)
       case While(c, b) =>
         check(voc, c)
         check(voc, b)
       case For(v,r,b) =>
         checkName(v) 
         check(voc, r)
         check(voc, b)
     }      
   }
}

// Relative semantics: translate the program to another language, whose semantics is already defined
object PythonTranslator {
   def print(sf: SyntaxFragment): String = {
     sf match {
       case Program(defs,mn) =>
         defs.map(print(_)+"\n").mkString("\n") + "\n" + print(mn)
       case FunDef(name, varnames, body) => 
         "def " + name + "(" + varnames.mkString(",") + "):\n" + print(body).indent(2)
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
       case VarDecl(n, iv) => n + " = " + print(iv)
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
      // parse an example program
      val prog = new Parser("""
def sum(x,y) = if ((x > y))
    return 0
  else {
    var n = x;
    var s = 0;
    while ((n<y)) {
      s = (s+n);
      n=(n+1)
    };
    return s
  }
def average(x,y) = return (sum(x,y)/(y-x))
def test() = {
  var x = int(input("Enter first number"));
  var y = int(input("Enter second number"));
  return sum(x,y)
}
print(test())
"""
      ).parseProgram
      
      // alternatively, we manually construct the parse result
      /*
      val prog = Program(List(
        FunDef("f", List("x", "y"),
           Return(
              InfixOperatorApply("+", Var("x"), InfixOperatorApply("*", IntegerLiteral(2), Var("y")))
           )
        ),
        FunDef("g", List("x"),
           Return(
              FunApply("f", List(Var("x"), IntegerLiteral(1)))
           )
        )
      ),
        Print(FunApply("f", List(IntegerLiteral(1), IntegerLiteral(2)))
      )) */


      // check the syntax
      Checker.check(Vocabulary(Nil), prog)

      // print the syntax
      // println(prog.print())
      // println(Printer.print(prog))

      // translate to Python
      println(PythonTranslator.print(prog))
   }
}