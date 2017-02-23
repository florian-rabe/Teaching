package language

/* Scala tricks to tweak inductive data types:
  - add "sealed" to each data type to get pattern-matching with exhaustiveness-checking
  - add "case" to each constructor to get the right behavior for equality and pattern-matching
*/


// contexts
case class Context(decls: List[Decl]) {
  // convenience functions to build a new context with additional declarations
  def and(d: Decl) = Context(decls ::: List(d))
  def and(ds: List[Decl]) = Context(decls ::: ds)
  
  // retrieve the most recent declaration for n
  def get(n: Name): Option[Decl] = decls.reverseIterator.find(d => d.name == n)
}

// declarations
sealed abstract class Decl {
  // require every declaration to have a name
  def name: Name
}
// variable definition; value is omitted for local assumptions
case class Val(name: Name, tp: Type, value: Option[Term]) extends Decl
// no type definitions---that makes the language too hard to implement
//TODO other cases: data types if you feel ambitious

// types
sealed abstract class Type
case class TypeRef(name: Name) extends Type
case class Int() extends Type
case class Bool() extends Type
case class FunType(from: Type, to: Type) extends Type
//TODO other cases: Product, base types

//terms
sealed abstract class Term
case class TermRef(name: Name) extends Term
case class IntLit(value: scala.Int) extends Term
case class Operator(op: String, args: List[Term]) extends Term // application of built-in operators
case class LocalDecl(decl: Decl, term: Term) extends Term
case class Lambda(argName: Name, argType: Type, body: Term) extends Term
case class Apply(fun: Term, args: Term) extends Term
//TODO other cases: Pair, Projection, booleans
//TODO statements

// objects are like classes in which all fields are static
object Operator {
  def builtInInfixOperators = List(",", "+", "-", "*", "div", "mod", "&&", "||", "==", "!=", "<=", ">=")
}

case class Name(name: String)