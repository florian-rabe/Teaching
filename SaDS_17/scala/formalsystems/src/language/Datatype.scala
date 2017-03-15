package language

/* Scala basics
   - class declarations
     - class NAME(ARGS) extends SUPERCLASS { FIELDS }
     - have exactly one constructor whose arguments are ARGS
     - fields can be functions 'def', immutable fields 'val', and mutable fields 'var'
     - fields may be 'private', default is 'public'
   
   - function declarations
     - def NAME(ARGS): RETURNTYPE = {BODY}
     - RETURNTYPE is usually optional
     - BODY contains local declarations ('def', 'val', 'var'), followed finally by the term to be returned 
     - 'return' is needed only when returning from the middle of the BODY
     - while (CONDITION) {BODY} works as usual
     - if (CONDITION) {THEN} else {ELSE} works as usual - but THEN and ELSE are terms like BODY

   - term declarations
     - val NAME : TYPE = TERM
     - 'val' is omitted in function and constructor arguments
     - 'var' instead of 'val' for mutable variables
     - TYPE is usually optional

   - objects instead of static fields
     - Every class is split into 'class' and optional 'object' declaration of the same name.
     - All static methods go into the object.
     - Fields of the object are like global definitions; fields of the class must be called on an instance of the class.

   - built-in types
     - Int, Boolean, String
     - functions: TYPE => TYPE with (NAME:TYPE) => TERM and TERM(TERMS)
     - products: (TYPE,TYPE) with (TERM,TERM) and TERM._1, TERM._2
     - lists: LIST[TYPE] with List(TERM,...,TERM) and TERM(INT) (see online API for available methods)
     - option: Option[TYPE] with Some(TERM) and None (see online API for available methods)
*/

/* Scala tricks to tweak inductive data types:
  - add "sealed" to each abstract class to get exhaustiveness-checking when pattern-matching
  - add "case" to each constructor to get the pattern-matching and the right behavior for equality
*/

/** contexts */
case class Context(decls: List[Decl]) {
  // convenience functions to build a new context with additional declarations
  def and(d: Decl) = Context(decls ::: List(d))
  def and(ds: List[Decl]) = Context(decls ::: ds)
  
  // retrieve the most recent declaration for n
  def get(n: Name): Option[Decl] = decls.reverseIterator.find(d => d.name == n)
}

// ***************************************************************** Names

/** names (We allow arbitrary strings here, but the parser will accept much less.) */
case class Name(name: String)

// ***************************************************************** Declarations

/** declarations */
sealed abstract class Decl {
  // require every declaration to have a name
  def name: Name
}
/** variable definition; value is omitted for local assumptions */
case class Val(name: Name, tp: Option[Type], value: Option[Term]) extends Decl

/** type assumptions needed for IDTDecl and ADTDecl */
case class TypeDecl(name: Name, value: Option[Type]) extends Decl


// ***************************************************************** Types

/** types */
sealed abstract class Type
case class TypeRef(name: Name) extends Type

sealed abstract class BaseType extends Type // convenience for grouping all base types together
case class Void() extends BaseType
case class Unit() extends BaseType
case class Int() extends BaseType
case class Bool() extends BaseType

case class FunType(from: Type, to: Type) extends Type
//TODO product types, more base types

// ***************************************************************** Terms

/** terms */
sealed abstract class Term

/** names **/
case class TermRef(name: Name) extends Term

/** unit literal */
case class UnitLit() extends Term
/** boolean literals */
case class BoolLit(value: Boolean) extends Term
/** integer literals */
case class IntLit(value: scala.Int) extends Term
/** unifies all built-in operators for the base types */
case class Operator(op: String, args: List[Term]) extends Term
/** if-then-else */
case class If(cond: Term, then: Term, els: Term) extends Term

/** local declaration in a term */
case class LocalDecl(decl: Decl, term: Term) extends Term

/** lambda abstraction */
case class Lambda(argName: Name, argType: Option[Type], body: Term) extends Term

/** function application */
case class Apply(fun: Term, args: Term) extends Term

//TODO pairs and projections for product types


object Operator {
  /** binary infix operators */
  def builtInInfixOperators = List("+", "-", "*", "div", "mod", "&&", "||", "==", "!=", "<=", ">=", "<", ">")
  /** other operators and their arity */
  def builtInOtherOperators = List(("!", 1))
}

// ***************************************************************** extensions for programming languages

// Disclaimer: I'm trying to implement this both systematically and easily-understandable.
//  That's very hard to combine, and I take some shortcuts that make some other things trickier.

// ******************************************************************** anonymous declarations

/** a term in declaration-position that is evaluated for its side-effect, but whose result can be thrown away */
case class Command(term: Term) extends Decl {
  def name = Name("") // anonymous
}

// ******************************************************************** non-termination

case class While(cond: Term, body: Term) extends Term

case class RecursiveVal(name: Name, tp: Type, df: Term) extends Decl

// ******************************************************************* mutable variables

case class Var(name: Name, tp: Option[Type], init: Term) extends Decl

case class Assignment(loc: Term, value: Term) extends Term

/** special terms for the run-time representation of the value of a mutable variable */
abstract class Location extends Term

// ******************************************************************** I/O

case class Print(term: Term) extends Term
case class Read() extends Term


// ******************************************************************** control flow operators (jumps that do not return)

sealed abstract class ControlFlowCommand extends Term // convenience for grouping all terms that are control flow operators

case class Return(value: Term) extends ControlFlowCommand
case class Break() extends ControlFlowCommand
case class Continue() extends ControlFlowCommand
case class Throw(exception: Term) extends ControlFlowCommand

case class Try(value: Term, handler: Term) extends Term

// ******************************************************************** extensions for data types

/** inductive data types */
case class IDTDecl(name: Name, constructors: List[Cons]) extends Decl
case class Cons(name: Name, argType: Type)

case class ConsApply(name: Name, argument: Term) extends Term

case class Match(term: Term, cases: List[ConsCase]) extends Term
case class ConsCase(name: Name, patvar: Name, argType: Option[Type], body: Term)

/** abstract data types (classes) */
case class ADTDecl(name: Name, fields: List[Field]) extends Decl
case class Field(name: Name, tp: Type)

case class New(cls: Name, definitions: List[FieldDef]) extends Term

case class FieldDef(name: Name, tp: Option[Type], definition: Term)
case class FieldAccess(instance: Term, field: Name) extends Term

/** special terms for the run-time representation of an instance of a class */
case class Instance(cls: Name, definitions: Context) extends Term

