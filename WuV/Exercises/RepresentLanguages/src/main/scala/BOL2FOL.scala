package wuv.exercise2

/** some ideas about the translation from BOL to FOL that were discussed in the lecture in response to strudent questions */

object FOL {
  sealed abstract class Formula
  case class Forall(variable: String, body: Formula) extends Formula
  case class Impl(left: Formula, right: Formula) extends Formula
  case class Atom(predicate: String, args: List[Term]) extends Formula
  sealed abstract class Term
  case class Variable(name: String) extends Term

  /** capture-avoiding substitution */
  def sub(f: Formula, n: String, t: Term): Formula = ???
}

/** using meta-level (= Scala) functions to represent free variables */
object BOL2FOLFunctions {
  private var num = 0
  private def next = {num += 1; "x_" + num.toString}
  def apply_Concept(c: BOL.ConceptExpr): FOL.Term => FOL.Formula = x => c match {
    case BOL.AtomicConcept(n) => FOL.Atom(n.id, List(x))
    case BOL.Forall(r,c) =>
      val rT = apply_Relation(r)
      val cT = apply_Concept(c)
      val freshName = next
      val f = FOL.Variable(freshName)
      FOL.Forall(freshName, FOL.Impl(rT(x, f), cT(f)))
  }
  def apply_Relation(r: BOL.RelationExpr): (FOL.Term, FOL.Term) => FOL.Formula = {case (x,y) => r match {
    case BOL.AtomicRelation(r) => FOL.Atom(r.id, List(x,y))
  }}
}

/** using open terms to represent free variables */
object BOL2FOLOpenTerms {
  def apply_Concept(c: BOL.ConceptExpr): (String,FOL.Formula) = c match {
    case BOL.AtomicConcept(n) => ("x", FOL.Atom(n.id, List(FOL.Variable("x"))))
    case BOL.Forall(r,c) =>
      val (x,y,rT) = apply_Relation(r)
      val (b, cT) = apply_Concept(c)
      val name = "x"
      val v = FOL.Variable(name)
      val f = FOL.Forall(name, FOL.Impl(FOL.sub(rT,y,v), FOL.sub(cT,b,v)))
      (x, f)
  }
  def apply_Relation(r: BOL.RelationExpr): (String,String,FOL.Formula) = r match {
    case BOL.AtomicRelation(r) => ("x","y", FOL.Atom(r.id, List(FOL.Variable("x"),FOL.Variable("y"))))
  }
}

/** FOL using fake HOAS using the Scala function space */
object FOLHOAS {
  sealed abstract class Formula
  case class Forall(body: Term => Formula) extends Formula
  case class Impl(left: Formula, right: Formula) extends Formula
  case class Atom(predicate: String, args: List[Term]) extends Formula
  sealed abstract class Term
  case class Variable(name: String) extends Term

  /** capture-avoiding substitution */
  def sub(f: Formula, n: String, t: Term): Formula = ???
}

/** FOL using de Bruijn indices */
object FOLdeBruijn {
  sealed abstract class Formula
  case class Forall(body: Formula) extends Formula
  case class Impl(left: Formula, right: Formula) extends Formula
  case class Atom(predicate: String, args: List[Term]) extends Formula
  sealed abstract class Term
  case class Variable(index: Int) extends Term

  /** refers to the innermost visible bound variable */
  val v0 = FOLdeBruijn.Variable(0)
  /** refers to the innermost-but-one visible bound variable */
  val v1 = FOLdeBruijn.Variable(1)

  /* substitute for variables 0, 1, ... */
  def sub(f: Formula, t: Term*): Formula = ???
}

object BOL2FOLdeBruijn {
  import FOLdeBruijn._

  def apply_Concept(c: BOL.ConceptExpr): Formula = c match {
    case BOL.AtomicConcept(n) => Atom(n.id, List(FOLdeBruijn.v0))
    case BOL.Forall(r,c) =>
      val rT = apply_Relation(r)
      val cT = apply_Concept(c)
      val f = Forall(Impl(sub(rT,v1,v0), sub(cT,v0)))
      f
  }
  def apply_Relation(r: BOL.RelationExpr): Formula = r match {
    case BOL.AtomicRelation(r) => Atom(r.id, List(v0,v1))
  }
}