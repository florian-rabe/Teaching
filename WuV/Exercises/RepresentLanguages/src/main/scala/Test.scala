package wuv.languages.example

object Test {
  val a1 = Sum(One(),One())
  val a2 = Product(a1,a1)
  val a3 = Ordering(a1,a2)

  val a = List(a1,a2,a3)

  def main(args: Array[String]) {
    println("concrete syntax")
    a.foreach(x =>
      println(a.toString)
    )

    println("\nScala semantics")
    val a1S = ScalaSemantics.translate_N(a1)
    val a2S = ScalaSemantics.translate_N(a2)
    val a3S = ScalaSemantics.translate_F(a3) // a3 is an F, not an N like a1 and a2
    List(a1S,a2S,a3S).foreach(x => println(x))

    println("\nSB semantics")
    val a1SB = SBSemantics.translate_N(a1)
    val a2SB = SBSemantics.translate_N(a2)
    val a3SB = SBSemantics.translate_F(a3) // a3 is an F, not an N like a1 and a2
    List(a1SB,a2SB,a3SB).foreach(x => println(x.toString)) // using auto-generated toString methods
  }
}

/** expected output:
concrete syntax
List(1+1, 1+1*1+1, 1+1<=1+1*1+1)
List(1+1, 1+1*1+1, 1+1<=1+1*1+1)
List(1+1, 1+1*1+1, 1+1<=1+1*1+1)

Scala semantics
||
||||
true

SB semantics
Character(|,Character(|,Empty()))
Character(|,Character(|,Character(|,Character(|,Empty()))))
True()
*/