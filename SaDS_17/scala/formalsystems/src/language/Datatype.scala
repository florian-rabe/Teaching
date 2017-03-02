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

/** names (We allow arbitrary strings here, but the parser will accept much less.) */
case class Name(name: String)

/** declarations */
sealed abstract class Decl {
  // require every declaration to have a name
  def name: Name
}
/** variable definition; value is omitted for local assumptions */
case class Val(name: Name, tp: Type, value: Option[Term]) extends Decl
// no type definitions---that makes the language harder to implement
//TODO data types if you feel ambitious

/** types */
sealed abstract class Type
case class TypeRef(name: Name) extends Type
case class Unit() extends Type
case class Int() extends Type
case class Bool() extends Type
case class FunType(from: Type, to: Type) extends Type
//TODO product types, more base types

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
/** convenience for built-in operators for the base types */
case class Operator(op: String, args: List[Term]) extends Term

/** local declaration in a term */
case class LocalDecl(decl: Decl, term: Term) extends Term

/** lambda abstraction */
case class Lambda(argName: Name, argType: Type, body: Term) extends Term

/** function application */
case class Apply(fun: Term, args: Term) extends Term

//TODO pairs and projections for product types


object Operator {
  /** the list of infix operators */
  def builtInInfixOperators = List(",", "+", "-", "*", "div", "mod", "&&", "||", "==", "!=", "<=", ">=")
}