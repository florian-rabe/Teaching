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

// E ::= I
// reference to variable called 'name'
case class Var(name: String) extends Expression {
   def print() = name
}

// E ::= N
// integer literal of value 'value'
case class IntegerLiteral(value: Int) extends Expression {
   def print() = value.toString()
}

// E ::= print(E)
case class Print(e: Expression) extends Expression {
   def print() = "print(" + e.print() + ")"
}

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
       case Var(n) => n
       case IntegerLiteral(v) => v.toString()
       case Print(e) => "print(" + print(e) + ")"
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
  
   val infixOperators = List("+", "*", "-", "&&", "||", "/")

   val forbiddenNames = List("def", "for", "in", "while", "if", "else", "print", "return")
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
         voc.lookup(n) match {
            case None => throw SyntaxError("name " + n + " not declared")
            case Some(FunDef(_, vrs, bd)) => 
              if (vrs.length != as.length)
                throw SyntaxError("function " + n + " applied to wrong number of arguments" +
                     " (expected: " + vrs.length + "; found: " + as.length + ")")
              as.foreach(check(voc, _))
            case Some(_) => throw SyntaxError("name " + n + " exists but does not refer to a function")
         }
         
       case InfixOperatorApply(o,l,r) =>
         if (!infixOperators.contains(o)) throw SyntaxError("unknown operator")
         check(voc, l)
         check(voc, r)
       case Var(n) =>
         // TODO: where applicable, throw SyntaxError("variable " + n + " not in scope")
       case IntegerLiteral(v) =>
       case Print(e) =>
         check(voc, e)
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


object Main {
   def main(args: Array[String]) = {
      // example program:
      // def f(x,y) =
      //   return x + 2*y
      // def g(x) = 
      //   return f(x,1)
      // print(f(z,2))
      
      // if we already had a parser, we would just call it this way
      // val prog = parse("def f(x,y):\n  return x + 2*y\nprint(f(1,2))")
      
      // instead, we manually construct the parse result for now
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
        Print(FunApply("f", List(Var("z"), IntegerLiteral(2)))
      ))

      // check the syntax
      Checker.check(Vocabulary(Nil), prog)

      // print the syntax
      // println(prog.print())
      println(Printer.print(prog))
   }
}