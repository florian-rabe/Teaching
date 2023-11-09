// Java minus the boilerplate

abstract class NonTerminal {
    def print(): String
}

abstract class N extends NonTerminal {
}
abstract class F extends NonTerminal {
}

/* Each class for a constructor, gets the "case" keyword, which
   - auto-generates the overriding of equals and hashCode
   - auto-generates selector (getter) methods of the same name for every constructor argument
   - allows ommitting "new" when constructing an object
*/

case class Sum(left: N, right: N) extends N {
  def print() = left.print() + "+" + right.print()
}  

case class Product(left: N, right: N) extends N {
  def print() = left.print() + "*" + right.print()
}  

case class Zero() extends N {
  def print() = "zero"
}

case class One() extends N {
  def print() = "one"
}

case class Equals(left: N, right: N) extends F {
  def print() = left.print() + "=" + right.print()
}  

case class LessEq(left: N, right: N) extends F {
  def print() = left.print() + "<=" + right.print()
}


object Test {
    def main(args: Array[String]) = {
        val x = Sum(Zero(), One())
        val y = Sum(Zero(), One())
        val z = LessEq(x,y)
        System.out.println(x == y)
        System.out.println(z.print())
    }
}
