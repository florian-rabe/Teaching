// a context-sensitive language with variable binding and a type-checker

// ******************************* Context-Free Syntax 

abstract class NonTerminal {
    def print(): String
}

// abstract class Option[A]
// case class Some[A](a: A) extends Option[A]
// case class None[A] extends Option[A]

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

// NEW
case class VarDecl(name: String, tp: Type) extends NonTerminal {
  // print as "name: tp"
  def print() = name + ": " + tp.print()
}

// NEW
case class Context(decls: List[VarDecl]) extends NonTerminal {
  // print contexts as ,-separated list
  def print() = decls.map(_.print()).mkString(", ")
  // note that we return the last (i.e., inner-most) declaration with the right name
  // any other declarations are shadowed and unretrievable
  def lookup(name: String): Option[VarDecl] = decls.reverse.find(_.name == name) 

  def extend(vd: VarDecl) = Context(decls ::: List(vd))
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
// NEW: references to variables
case class VarReference(name: String) extends Expr {
  def print() = name
}

abstract class Form extends NonTerminal
case class PredicateSymbolReference(name: String, arguments: List[Expr]) extends Form {
  // print as "name(arguments)"
  def print() = name + "(" + arguments.map(_.print()).mkString(",") + ")"
}

// NEW: an example of a binder
case class UnivQuant(vd: VarDecl, body: Form) extends Form {
  def print() = "forall " + vd.print() + ". " + body.print()
}

// ************************ Context-Sensitive Type-Checker

/* Now we extend the type-checker to one with variable contexts

   The check functions for expressions take a context as an additional argument.
*/

// In Scala, an object is like a class with only static members.
object TypeChecker {
  val emptyCon = Context(Nil)
  
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

  // NEW: context for expression checking initialized to be empty
  def check_Decl(voc: Vocabulary, d: Declaration): Boolean = {
    d match {
      case FunctionSymbolDeclaration(n,ins,out) =>
        if (voc.lookup(n).isDefined)
          false // in practice, throw "name already defined" error
        else {
           ins.forall(i => check_Type(voc, emptyCon, i)) && check_Type(voc, emptyCon, out)
        }
      case PredicateSymbolDeclaration(n,ins) =>
        if (voc.lookup(n).isDefined)
          false
        else {
           ins.forall(i => check_Type(voc, emptyCon, i))
         }
      case TypeSymbolDeclaration(n) =>
        if (voc.lookup(n).isDefined)
          false
        else {
          true
        }
    }
  }

  // NEW: contexts and variable declarations are checked like expressions
  def check_Context(voc: Vocabulary, ctx: Context, con: Context) : Boolean = {
    var ctxSofar = ctx
    con.decls.forall {vd => 
       val r = check_VarDecl(voc, ctxSofar, vd)
       ctxSofar = ctxSofar.extend(vd)
       r
    }
  }
  def check_VarDecl(voc: Vocabulary, ctx: Context, vd: VarDecl) : Boolean = {
     check_Type(voc, ctx, vd.tp)
  }
  
  // types are not typed, so no expected type
  // NEW: context argument taken and passed on (technically not needed as long as variables can never in types)
  def check_Type(voc: Vocabulary, ctx: Context, tp: Type): Boolean = {
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
  // NEW: context argument taken and passed on
  def check_Expr(voc: Vocabulary, ctx: Context, e: Expr, expected: Type): Boolean = {
    e match {
       // NEW
       case VarReference(n) =>
         ctx.lookup(n) match {
           case None => false // in practice, throw undefined variable error
           case Some(vd) => 
               if (vd.tp != expected) {
                 false // in practice, return "expected X, found Y" error
               } else {
                 true
               }
         }
       case FunctionSymbolReference(s, args) =>
         voc.lookup(s) match {
           case None => false
           case Some(d) => d match {
             case FunctionSymbolDeclaration(_, ins,out) =>
               val checkIns = if (ins.length != args.length) {
                 false
               } else {
                 args.zip(ins).forall{case (a,i) => check_Expr(voc, ctx, a, i)}
               }
               if (!checkIns) return false
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
  // NEW: context argument taken and passed on
  def check_Form(voc: Vocabulary, ctx: Context, f: Form): Boolean = {
    f match {
       case PredicateSymbolReference(s, args) =>
         voc.lookup(s) match {
           case None => false
           case Some(d) => d match {
             case PredicateSymbolDeclaration(_, ins) =>
               if (ins.length != args.length) {
                 false
               } else {
                 args.zip(ins).forall{case (a,i) => check_Expr(voc, ctx, a, i)}
               }
             case _ => false
           }
         }
      // NEW
      case UnivQuant(vd, f) =>
         check_VarDecl(voc, ctx, vd) && check_Form(voc, ctx.extend(vd), f)
      
     }
  }
}


object Test {
    def main(args: Array[String]) = {
        // because we do not have a parser, we need to build some objects manually for testing
        
        // an example vocabulary
        // Nat: TYPE
        val natDecl = TypeSymbolDeclaration("Nat")
        val stringDecl = TypeSymbolDeclaration("String")
        val nat = TypeSymbolReference("Nat")
        val str = TypeSymbolReference("String")
        // 1: Nat, sum: Nat Nat -> Nat, lesseq: Nat Nat -> FORM
        val oneDecl = FunctionSymbolDeclaration("1", List(), nat)
        val one = FunctionSymbolReference("1", List())
        val sumDecl = FunctionSymbolDeclaration("sum", List(nat,nat), nat)
        val lesseqDecl = PredicateSymbolDeclaration("lessEq", List(nat,nat))
        val lengthDecl = FunctionSymbolDeclaration("length", List(str),nat)
        // fib: Nat -> Nat
        val fibDecl = FunctionSymbolDeclaration("fib", List(nat), nat)
        val voc = Vocabulary(List(natDecl, oneDecl, sumDecl, lesseqDecl, fibDecl, stringDecl, lengthDecl))
        System.out.println("vocabulary: " + voc.print()) 
        // check the vocabulary
        System.out.println(TypeChecker.check_Voc(voc))

        // example expressions relative to that vocabulary
        val xd = VarDecl("x", nat)
        val yd = VarDecl("y", nat)
        val x = VarReference("x")
        val y = VarReference("y")
        val s = FunctionSymbolReference("sum", List(x,one))
        val f = FunctionSymbolReference("fib", List(y))
        val l = PredicateSymbolReference("lessEq", List(s,f))
        val m = UnivQuant(yd, l)
        val ctx = Context(List(xd))
        System.out.println("context: " + ctx.print())
        System.out.println(TypeChecker.check_Context(voc, Context(Nil), ctx))

        System.out.println("checking " + s.print() + " : " + nat.print())
        System.out.println(TypeChecker.check_Expr(voc, ctx, s, nat))

        System.out.println("checking " + f.print() + " : " + nat.print())
        System.out.println(TypeChecker.check_Expr(voc, ctx.extend(yd), f, nat))
        
        System.out.println("checking formula " + m.print())
        // check the expressions      
        System.out.println(TypeChecker.check_Form(voc, ctx, m))
    }
}
