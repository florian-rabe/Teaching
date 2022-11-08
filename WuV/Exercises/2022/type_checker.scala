// a context-sensitive language with a type-checker

// ******************************* Context-Free Syntax 

abstract class NonTerminal {
    def print(): String
}

// At the toplevel, every formal language has a vocabulary, which is a list of declarations
// (we can skip the abstract class if the non-terminal has exactly one production)
case class Vocabulary(decls: List[Declaration]) extends NonTerminal {
  // print all declarations and concatenate with separator ", "
  def print() = decls.map(_.print()).mkString(", ")
}

// there can be many different kinds of declarations
abstract class Declaration extends NonTerminal {
  // most declarations have a name, but not all e.g., axioms
  def nameO: Option[String]
}
// the most common kind introduces a typed identifier
case class FunctionSymbolDeclaration(name: String, inputs: List[Type], output: Type) extends Declaration {
  def nameO = Some(name)
  // print as "name: inputs -> output"
  def print() = name + ": " + inputs.map(_.print()).mkString(" ") + " -> " + output.print()
}

// Compared to the lecture, I have removed the Type-BaseType distinction to simplify the language.

abstract class Type extends NonTerminal
case class Nat() extends Type {
  def print() = "Nat"
}
// more types as we like, e.g., Bool, String, etc.

// I've renamed Expr to Expr and F to Form
abstract class Expr extends NonTerminal {
}
abstract class Form extends NonTerminal {
}

// for every declaration, there is a production to refer to the declared identifer, i.e., to use it in an expression
case class FunctionSymbolReference(name: String, arguments: List[Expr]) extends Expr {
  // print as "name(arguments)"
  def print() = name + "(" + arguments.map(_.print()).mkString(",") + ")"
}

case class Sum(left: Expr, right: Expr) extends Expr {
  def print() = left.print() + "+" + right.print()
}  

case class Product(left: Expr, right: Expr) extends Expr {
  def print() = left.print() + "*" + right.print()
}  

case class Zero() extends Expr {
  def print() = "zero"
}

case class One() extends Expr {
  def print() = "one"
}

case class Equals(left: Expr, right: Expr) extends Form {
  def print() = left.print() + "=" + right.print()
}  

case class LessEq(left: Expr, right: Expr) extends Form {
  def print() = left.print() + "<=" + right.print()
}

// ************************ Context-Sensitive Type-Checker

/* A type-checker traverses the AST. Any such traversal consists of
   - one function for every non-terminal (mutually recursive)
   - one case for every constructor
   - one recursive call for every constructor argument

   We first do the simple case where we only have a single base type.
   In this case, the base types in the inputs and outputs of the symbols are always the same and only the number of arguments must be checked.

   In that case, the type-checker is a traversal that
   - takes a Vocabulary as an extra argument - that's the context that makes everything context-sensitive
   - returns Boolean
*/

// In Scala, an object is like a class with only static members.
object TypeChecker {
  
  // check every declaration relative to the vocabulary before it - this is the same for virtually every formal language
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
  
  def check_Decl(voc: Vocabulary, d: Declaration): Boolean = {
    d match {
      case FunctionSymbolDeclaration(n,ins,out) =>
        if (voc.decls.exists(e => e.nameO == Some(n)))
          false // in practice, throw "name already defined" error
        else {
           ins.forall(i => check_Type(voc, i)) && check_Type(voc, out)
         }
    }
  }

  // compared the the lecture, I've removed function types for simplification
  def check_Type(voc: Vocabulary, tp: Type) = {
    tp match {
      case Nat() => true
    }
  }

  // In our simplified variant, where we only check arity, expressions are well-typed if
  // all subexpressions are and applications use the right number of arguments.
  // We will expand on that later.
  def check_Expr(voc: Vocabulary, n: Expr): Boolean = {
     n match {
       
       case Zero() =>
         true
       
       case One() =>
         true
       
       case Sum(l,r) =>
         check_Expr(voc, l) && check_Expr(voc, r)
       
       case Product(l,r) =>
         check_Expr(voc, l) && check_Expr(voc, r)
       
       case FunctionSymbolReference(s, args) =>
         // for the reference to the identifiers in the vocabulary, we have to lookup the identifier in the vocabulary
         voc.decls.find(d => d.nameO == Some(s)) match {
           case None => false // in practice, throw "symbol not declared" error
           case Some(d) => d match {
             case FunctionSymbolDeclaration(_, ins,out) =>
               if (ins.length != args.length) {
                 false // in practice, throw "wrong number of argumetns" error
               } else {
                 // check every argument recursively
                 args.forall(a => check_Expr(voc, a))
               }
           }
         }
     }
    
  }
  
  def check_Form(voc: Vocabulary, f: Form): Boolean = {
    f match {
      case Equals(l,r) =>
         check_Expr(voc, l) && check_Expr(voc, r)
      case LessEq(l,r) =>
         check_Expr(voc, l) && check_Expr(voc, r)
    }
  }
}


object Test {
    def main(args: Array[String]) = {
        // because we do not have a parser, we need to build some objects manually for testing
        
        // an example vocabulary
        // fib: Nat -> Nat
        val fibDecl = FunctionSymbolDeclaration("fib", List(Nat()), Nat())
        val voc = Vocabulary(List(fibDecl))
        System.out.println(voc.print()) 
        // check the vocabulary
        System.out.println(TypeChecker.check_Voc(voc))  

        // example expressions relative to that vocabulary
        val x = Sum(Zero(), One())
        val y = FunctionSymbolReference("fib", List(x))
        val z = LessEq(x,y)
        System.out.println(z.print())
        // check the expressions      
        System.out.println(TypeChecker.check_Form(voc, z))
    }
}
