/* a fragment of BOL and a parser for it - not debugged */

case class Ontology(ds: List[Declaration])

abstract class Declaration {
    def print: String
}
case class ConcDecl(name: String) extends Declaration {
    def print = "concept " + name
}
case class IndDecl(name: String) extends Declaration {
    def print = "individual " + name
}
case class Axiom(f: Formula) extends Declaration {
    def print = "axiom " + f.print()
}

abstract class Expression {
    def print: String
}

abstract class Individual extends Expression
case class IndRef(name: String) extends Individual {
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

abstract class Formula extends Expression 
case class ConcAss(ind: Individual, conc: Concept) extends Formula {
    def print = "isa " + ind.print() + conc.print()
}
case class Subsume(left: Concept, right: Concept) extends Formula {
    def print = "sub " left.print() + right.print()
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