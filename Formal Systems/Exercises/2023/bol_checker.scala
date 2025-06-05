case class Ontology(ds: List[Declaration]) {
    def print: String
    def lookupO(n: String): Option[Declaration] = {
        ds.find(d => d.nameO == Some(n))
    }
    def declares(n: String): Boolean = lookupO(n).isDefined
    def lookup(n: String): Declaration = lookupO(n).getOrElse {throw CheckError("unknown identifier: " + n)}
}

abstract class Declaration {
    def print: String
    def nameO: Option[String]
}
case class ConcDecl(name: String) extends Declaration {
    def print = "concept " + name
    def nameO = Some(name)
}
case class IndDecl(name: String) extends Declaration {
    def print = "individual " + name
    def nameO = Some(name)
}
case class RelDecl(name: String) extends Declaration {
    def print = "relation " + name
    def nameO = Some(name)
}
case class PropDecl(name: String, tp: Type) extends Declaration {
    def print = "property " + name + " : " + tp.print()
    def nameO = Some(name)
}
case class Axiom(f: Formula) extends Declaration {
    def print = "axiom " + f.print()
    def nameO = None
}

// Expressions

abstract class Expression {
    def print: String
}

abstract class Individual extends Expression
case class IndRef(name: String) extends Individual {
    def print = name
}

abstract class Property extends Expression
case class PropRef(name: String) extends Property {
    def print = name
}

abstract class Concept extends Expression
case class ConcRef(name: String) extends Concept {
    def print = name
}

case class Union(left: Concept, right: Concept) extends Concept {
    def print = "union " + left.print() + right.print()
}
case class Intersection(left: Concept, right: Concept) extends Concept {
    def print = "intersect " + left.print() + right.print()
}
case class Forall(rel: Relation, conc: Concept) extends Concept {
    def print = "forall " + rel.print() + conc.print()
}

abstract class Relation extends Expression
case class RelRef(n: String) extends Relation

abstract class Formula extends Expression 
case class Subsume(left: Concept, right: Concept) extends Formula {
    def print = "sub " left.print() + right.print()
}
case class ConcAss(ind: Individual, conc: Concept) extends Formula {
    def print = "isa " + ind.print() + conc.print()
}
case class RelAss(subj: Individual, rel: Relation, obj: Individual) extends Formula {
    def print = "rel " + subj.print() + rel.print() + obj.print()
}
case class PropAss(ind: Individual, prop: Property, value: Value) extends Formula {
    def print = "has " + ind.print() + prop.print() + value.print()
}

abstract class Type(s: String) extends Expression {
    def print = s
}
case class IntType() extends Type("int")
case class StringType() extends Type("string")
case class BoolType() extends Type("bool")

abstract class Value(v: Any) extends Expression {
    def print = v.toString
}
case class IntVal(v: Int) extends Value
case class StringVal(v: String) extends Value
case class BoolVal(v: Boolean) extends Value


object Checker {
    def checkOntology(ctx: Ontology, o: Ontology) {
        o.decls match {
            case Nil =>
            case d::rest =>
               checkDeclaration(ctx, d)
               checkOntology(Ontology(ctx.decls ::: List(d), Ontology(rest)))
        }
    }
    def checkDeclaration(ctx: Ontology, d: Declaration) {
        d.nameO match {
           case None => // unnamed, always allowed
           case Some(n) => if (ctx.declares(n)) throw CheckError("identifier already declared: " + n)
        }
    }
    def checkFormula(ctx: Ontology, f: Formula) {
        f match {
          case Subsume(c,d) =>
            checkConcept(ctx, c)
            checkConcept(ctx, d)
          case ConcAss(ind, c) =>
            checkIndividual(ctx, i)
            checkConcept(ctx, c)
          case RelAss(s, r, o) =>
            checkIndividual(ctx, s)
            checkRelation(ctx, r)
            checkIndividual(ctx, o)
          case PropAss(i, p, v) =>
            checkIndividual(ctx, i)
            val t = checkProperty(ctx, p)
            checkValue(ctx, v, t)
        }        
    }
    def checkConcept(ctx: Ontology, c: Concept) {
        c match {
            case Union(c,d) =>
               checkConcept(ctx, c)
               checkConcept(ctx, d)
            case ConcRef(n) => 
               val decl = ctx.loopup(n)
               decl match {
                   case _: ConcDecl => 
                   case _ => throw CheckError("expected concept; found " + n)
               }
        }
        
    }
    def checkIndividual(ctx: Ontology, i: Individual) {
        i match {
          case IndRef(n) =>
             val decl = ctx.loopup(n)
             decl match {
                 case _: IndDecl => 
                 case _ => throw CheckError("expected individual; found " + n)
             }
        }
    }
    def checkRelation(ctx: Ontology, r: Relation) {
        r match {
          case RelRef(n) =>
             val decl = ctx.loopup(n)
             decl match {
                 case _: RelDecl => 
                 case _ => throw CheckError("expected relation; found " + n)
             }
        }
    }
    def checkProperty(ctx: Ontolgoy, p: Property): Type = {
        p match {
          case PropRef(n) =>
             val decl = ctx.loopup(n)
             decl match {
                 case PropDecl(_,t) => t
                 case _ => throw CheckError("expected property; found " + n)
             }
    }
    def checkValue(ctx: Ontology, v: Value, t: Type) {
        val type_of_v = v match {
          case IntValue(_) => IntType()
          case BoolValue(_) => BoolType()
          case StringValue(_) => StringType()
        }
        if (type_of_v != t) {
          throw CheckError("expected " + t.print() + "; found " + type_of_v.print() + ": " + v.print())
        }
    }
}

/* every method parse_XXX
   - takes the input string to parse
   - parses an XXX from the beginning of the input
   - return the parsed XXX and the remainder of the input
*/
object Parser {
    def parse_Ontology(input: String): (Ontology,String) = {
        var rest = input
        var ds : List[Declaration] = Nil
        while (rest != "") {
            val (d,r) = parse_Declaration(rest)
            ds ::= d
            rest = r
        }
        (Ontology(ds.reverse), rest)
    }
    def parse_Name(input: String): (String,String) = {
        var i = 0
        while (input.chatAt(i).isLetter) {
            i += 1
        }
        if (i == 0) {
            throw ParseError("expect identifier; found " + input)
        }
        (input.substring(0,i), input.substring(i))
    }
    def parse_Declaration(input: String): (Declaration,String) = {
        if (input.startsWith("concept ")) {
            val r = input.drop(8)
            val (n,r2) = parse_Name(input)
            (ConcDecl(n), r2)
        } else if input.startsWith("individual ") {
            val r = input.drop(11)
            val (n,r2) = parse_Name(input)
            (IndDecl(n), r2)
        } else if input.startsWith("axiom ") {
            val r = input.drop(6)
            val (f,r2) = parse_Formula(input)
            (Axiom(f), r2)
        } else {
            throw ParseError("expected 'concept', 'individual', 'axiom'; found " + input)
        }
    }
    def parse_Individual(input: String): (Individual,String) = {
        val (n,r) = parse_Name(input)
        (IndRef(n), r)
    }
    def parse_Concept(input: String): (Concept,String) = {
        if (input.startsWith("union ")) {
            val r = input.drop(6)
            val (left,r2) = parse_Concept(r)
            val (right,r3) = parse_Concept(r2)
            (Union(left,right), r3)
        } else if (input.startsWith("intersection ")) {
            val r = input.drop(13)
            val (left,r2) = parse_Concept(r)
            val (right,r3) = parse_Concept(r2)
            (Intersection(left,right), r3)
        } else {
            val (n,r) = parse_Name(input)
            (ConcRef(n), r)
        }
    }
    def parse_Formula(input: String): (Formula,String) = {
        if (input.startsWith("isa ")) {
            val r = input.drop(4)
            val (ind,r2) = parse_Individual(r)
            val (conc,r3) = parse_Concept(r2)
            (ConcAss(ind,conc), r3)
        } else if (input.startsWith("sub ")) {
            val r = input.drop(4)
            val (left,r2) = parse_Concept(r)
            val (right,r3) = parse_Concept(r2)
            (Subsume(left,right), r3)
        } else {
            throw ParseError("expected formula; found " + input)
        }
    }
}


object Test {
    // parse an ontology from a file and print int
    def main(args: Array[String]): Int = {
        val f = new java.io.File(args(0))
        val input = readFile(f)
        val (o, r) = Parser.parse_Ontology(input)
        if (r != "") throw ParseError("extraneous input after parsing: " + r)
        Checker.checkOntology(o)
        println(o.print())
        0
    }
}

/* alternative parser implementation using
   - a global variable for the parsing process/rest/position
   - an integer for the rest instead of a string (avoids copying the string)
*/
class Parser(input: String) {
    private var index = 0
    def parseName : String = {
        val begin = index
        while (input.charAt(index).isLetter) {
            index += 1
        }
        input.substring(begin,index-begin)
    }
    
    def parseDeclaration : Declaration = {
        if (input.substring(index,8) = "concept ") {
            index += 8
            ConcDecl(parseName)
        } else {
            throw ParseError("expected declaration")
        }
    }
    def parseConcept : Concept = {
        if (input.substring(index,6) = "union ") {
            index += 6
            Union(parse_Concept, parse_Concept)    
        } else {
            ConcRef(parse_Name)
        }
    }
}