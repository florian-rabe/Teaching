package wuv.exercise2

/** the syntax of BOL that was implemented in the practice session */

object BOL {

  // the pre-existing types, here implemented from scratch
  case class ID(id: String) {
    override def toString = id
  }

  sealed abstract class Type
  case class IntType() extends Type {
    override def toString = "int"
  }
  case class StringType() extends Type {
    override def toString = "string"
  }

  sealed abstract class Value
  case class IntValue(value: Int) extends Value {
    override def toString = value.toString
  }
  case class StringValue(value: String) extends Value {
    override def toString = value.toString
  }

  // the theory part

  case class Ontology(decls: List[Declaration]) { // O ::= D^*
    override def toString = {
      // function that takes Declaration d and returns d.toString:   (d: Declaration) => d.toString
      // variable type can be inferred: d => d.toString
      // short-cut for lambda-abstraction that uses the variable only once: _.toString
      val declStrings = decls.map(_.toString)
      declStrings.mkString("[", "\n", "]")  // stringList.mkString(before, between, after)

      /* if we just wrote "decls.mkString("[","\n","]")",
         the Scala type checker would insert the .map(_.toString) automatically
         because toString is an implicit function;
         those can be omitted if they can be inferred during type-checking
       */
    }
  }
  sealed abstract class Declaration // D ::=

  sealed abstract class NamedDeclaration(keyword: String) extends Declaration {
    val name: ID
    override def toString = keyword + " " + name.toString
  }

  case class IndividualDecl(name: ID) extends NamedDeclaration("individual") {// :: = individual ID
  }
  case class ConceptDecl(name: ID) extends NamedDeclaration("concept") { // :: = concept ID
  }
  case class RelationDecl(name: ID) extends NamedDeclaration("relation") { // :: = relation ID
  }
  case class PropertyDecl(name: ID, tp: Type) extends NamedDeclaration("property") { // :: = property ID
  }

  // the expression part

  sealed abstract class Formula // F ::=

  case class Subsumption(con1: ConceptExpr, con2: ConceptExpr) extends Formula {// ::= C1 \sqsubseteq C2
    override def toString = con1.toString + " sub " + con2.toString
  }

  sealed abstract class ConceptExpr   // C ::=
  case class AtomicConcept(name: ID) extends ConceptExpr { // ::= ID
    override def toString = name.toString
  }
  case class UnionConcept(con1: ConceptExpr, con2: ConceptExpr) extends ConceptExpr { // ::= C \cup C
    override def toString = "(" + con1.toString + " cup " + con2.toString + ")"
  }
  case class Forall(r: RelationExpr,c: ConceptExpr) extends ConceptExpr { // ::= \forall R.C
    override def toString = "forall " + r.toString + "." + c.toString
  }
  sealed abstract class RelationExpr // R ::=
  case class AtomicRelation(name: ID) extends RelationExpr { // ::= ID
    override def toString = name.toString
  }
  case class UnionRelation(rel1: RelationExpr, rel2: RelationExpr) extends RelationExpr { // ::= R \cup R
    override def toString = "(" + rel1.toString + " cup " + rel2.toString + ")"
  }
}

// the semantics of BOL in Scala, i.e., a translation from BOL to Scala

class BOL2Scala {
  // get rid of "BOL." prefix
  import BOL._
  import scala.collection.mutable.{HashSet,HashMap}

  val individuals = new HashSet[ID]
  val concepts = new HashMap[ID,HashSet[ID]]
  val relations = new HashMap[ID,HashSet[(ID,ID)]]
  val properties = new HashMap[ID,(Type,HashSet[(ID,Value)])]

  // one inductive function per non-terminal
  /* 3 options for return value:
     a) Scala syntax (using scala.compiler library)
     b) Strings holding Scala syntax (that's the one from the lecture notes)
     c) execute Scala code directly (only possible because we implement the semantics in the target languge itself)ß
   */
  def applyOntology(o: Ontology) {
    // no case distinction needed because there is only one case
    o.decls.foreach(d => applyDeclaration(d))
  }

  def applyDeclaration(d: Declaration) {
    // one case per production
    d match {
      case IndividualDecl(id) =>
        individuals += id
      case ConceptDecl(id) =>
        val hs = new HashSet[ID]
        concepts(id) = hs // Scala parser turns this into concepts.update(id,hs)
      case RelationDecl(id) =>
        val hs = new HashSet[(ID,ID)]
        relations(id) = hs
      case PropertyDecl(id,tp) =>
        val hs = new HashSet[(ID,Value)]
        properties(id) = (tp,hs)
    }
  }

  // formulas are translated to Booleans, true if the formula holds about the ontology
  def applyFormula(f: Formula): Boolean = {
    f match {
      case Subsumption(c1, c2) =>
        val c1Sem = applyConcept(c1)
        val c2Sem = applyConcept(c2)
        c1Sem.forall(i => c2Sem.contains(i))
    }
  }

  // formulas are translated to HashSets containing the ids of the individuals in the concept
  def applyConcept(c: ConceptExpr): HashSet[ID] = {
    c match {
      case AtomicConcept(id) =>
        concepts(id)  // Scala parser turns this into concepts.apply(id)
      case UnionConcept(c1,c2) =>
        val c1Sem = applyConcept(c1)
        val c2Sem = applyConcept(c2)
        val hs = new HashSet[ID]
        c1Sem.foreach(i => hs += i)
        c2Sem.foreach(i => hs += i)
        hs
    }
  }
}

// an executable program; create Run configuration with class wuv.exercise2.Test
object Test {
  import BOL._
  def main(args: Array[String]) {
    // an example ontology
    val d1 = ConceptDecl(ID("Instructor"))
    val d2 = ConceptDecl(ID("Course"))
    val d3 = IndividualDecl(ID("FlorianRabe"))
    val d4 = RelationDecl(ID("teach"))
    val o = Ontology(List(d1,d2,d3,d4))

    // the semantics of o
    val sem = new BOL2Scala
    sem.applyOntology(o)
    // test with one example concept
    val c = UnionConcept(AtomicConcept(ID("Instructor")),AtomicConcept(ID("Course")))
    val cSem = sem.applyConcept(c)
    cSem.foreach(i => println(i))
    // test with one example formula
    val f = Subsumption(AtomicConcept(ID("Instructor")),AtomicConcept(ID("Course")))
    println(sem.applyFormula(f))
  }
}