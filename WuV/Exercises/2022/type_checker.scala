// a context-sensitive language with a type-checker
// for simplicity, we use the variant of the example language in which primitive features are shifted to the standard library

// ******************************* Context-Free Syntax 

abstract class NonTerminal {
    def print(): String
}

case class Vocabulary(decls: List[Declaration]) extends NonTerminal {
  def print() = decls.map(_.print()).mkString(", ")

  def lookup(name: String): Option[Declaration] = decls.find(_.nameO == Some(name))
}

// there can be many different kinds of declarations
abstract class Declaration extends NonTerminal {
  def nameO: Option[String]}

case class FunctionSymbolDeclaration(name: String, inputs: List[Type], output: Type) extends Declaration {  def nameO = Some(name)
  // print as "name: inputs -> output"
  def print() = name + ": " + inputs.map(_.print()).mkString(" ") + " -> " + output.print()
}

case class PredicateSymbolDeclaration(name: String, inputs: List[Type]) extends Declaration {
  def nameO = Some(name)
  // print as "name: inputs -> FORM"
  def print() = name + ": " + inputs.map(_.print()).mkString(" ") + " -> " + "FORM"
}

case class TypeSymbolDeclaration(name: String) extends Declaration {
  def nameO = Some(name)
  // print as "name: TYPE"
  def print() = name + ": TYPE"
}


abstract class Type extends NonTerminal {}
case class TypeSymbolReference(name: String) extends Type {
  def print() = name
}

abstract class Expr extends NonTerminal {}
case class FunctionSymbolReference(name: String, arguments: List[Expr]) extends Expr {
  // print as "name(arguments)"
  def print() = name + "(" + arguments.map(_.print()).mkString(",") + ")"
}

abstract class Form extends NonTerminal
case class PredicateSymbolReference(name: String, arguments: List[Expr]) extends Form {
  // print as "name(arguments)"
  def print() = name + "(" + arguments.map(_.print()).mkString(",") + ")"
}

// ************************ Context-Sensitive Type-Checker

/* Now we extend the syntax checker to a type-checker

   It takes additional arguments for the expected type for every non-terminal for which typing is used.
*/

// In Scala, an object is like a class with only static members.
object TypeChecker {
  
  // unchanged
  def check_Voc(voc: Vocabulary): Boolean = {
    var seenSofar: List[Declaration] = List()
    voc.decls.forall {d =>
       // check the current declaration relative to the ones seen before
       val r = check_Decl(Vocabulary(seenSofar), d)
       // append it to the list of seen declarations
       seenSofar = seenSofar ::: List(d)
       r
    }
  }

  // unchanged
  def check_Decl(voc: Vocabulary, d: Declaration): Boolean = {
    d match {
      case FunctionSymbolDeclaration(n,ins,out) =>
        if (voc.lookup(n).isDefined)
          false // in practice, throw "name already defined" error
        else {
           ins.forall(i => check_Type(voc, i)) && check_Type(voc, out)
        }
      case PredicateSymbolDeclaration(n,ins) =>
        if (voc.lookup(n).isDefined)
          false
        else {
           ins.forall(i => check_Type(voc, i))
         }
      case TypeSymbolDeclaration(n) =>
        if (voc.lookup(n).isDefined)
          false
        else {
          true
        }
    }
  }

  // types are not typed, so no expected type
  def check_Type(voc: Vocabulary, tp: Type): Boolean = {
    tp match {
      case TypeSymbolReference(s) =>
        voc.lookup(s) match {
          case Some(d) => d match {
            case TypeSymbolDeclaration(_) => true
            case _ => false
          }
          case _ => false
        }   
    }
  }

  // Expr-expressions are typed by Type-expressions
  def check_Expr(voc: Vocabulary, n: Expr, expected: Type): Boolean = {
    n match {
       case FunctionSymbolReference(s, args) =>
         voc.lookup(s) match {
           case None => false
           case Some(d) => d match {
             case FunctionSymbolDeclaration(_, ins,out) =>
               if (ins.length != args.length) {
                 false
               } else {
                 args.zip(ins).forall{case (a,i) => check_Expr(voc, a, i)}
               }
               // more generally, there might be a function for checking that two types are equal, which is less trivial
               // in fact, in geneal, it is function that checkes that out <: expected
               // e.g., for type abbreviations, subtyping, or non-trivial type equalities
               if (out != expected) {
                 false // in practice, return "expected X, found Y" error
               } else {
                 true
               }
             case _ => false
           }
         }
     }
  }

  // formulas are not typed, so no expected type
  def check_Form(voc: Vocabulary, f: Form): Boolean = {
    f match {
       case PredicateSymbolReference(s, args) =>
         voc.lookup(s) match {
           case None => false
           case Some(d) => d match {
             case PredicateSymbolDeclaration(_, ins) =>
               if (ins.length != args.length) {
                 false
               } else {
                 args.zip(ins).forall{case (a,i) => check_Expr(voc, a, i)}
               }
               true
             case _ => false
           }
         }
     }
  }
}


object Test {
    def main(args: Array[String]) = {
        // because we do not have a parser, we need to build some objects manually for testing
        
        // an example vocabulary
        // Nat: TYPE
        val natDecl = TypeSymbolDeclaration("Nat")
        val nat = TypeSymbolReference("Nat")
        // 1: Nat, sum: Nat Nat -> Nat, lesseq: Nat Nat -> FORM
        val oneDecl = FunctionSymbolDeclaration("1", List(), nat)
        val one = FunctionSymbolReference("1", List())
        val sumDecl = FunctionSymbolDeclaration("sum", List(nat,nat), nat)
        val lesseqDecl = PredicateSymbolDeclaration("lessEq", List(nat,nat))
        // fib: Nat -> Nat
        val fibDecl = FunctionSymbolDeclaration("fib", List(nat), nat)
        val voc = Vocabulary(List(natDecl, oneDecl, sumDecl, lesseqDecl, fibDecl))
        System.out.println(voc.print()) 
        // check the vocabulary
        System.out.println(TypeChecker.check_Voc(voc))

        // example expressions relative to that vocabulary
        val x = FunctionSymbolReference("sum", List(one,one))
        val y = FunctionSymbolReference("fib", List(x))
        val z = PredicateSymbolReference("lessEq", List(x,y))
        System.out.println(TypeChecker.check_Expr(voc, x, nat))
        System.out.println(TypeChecker.check_Expr(voc, y, nat))
        System.out.println(z.print())
        // check the expressions      
        System.out.println(TypeChecker.check_Form(voc, z))
    }
}
