package wuv.languages.example

/** a semantics that interprets the syntax directly as Scala objects
  * Thus, Scala itself is the target language.
  */

/** the basic operations in Scala */
/** methods in an object are globally available via qualified names, corresponds to static methods in Java */
object ScalaOperations {
  def conc(x:String, y: String): String = x+y
  def replace(x:String, c: Char, y:String): String = {
    x.flatMap(i => if (i == c) y else i.toString)
  }
  def equal(x:String, y:String): Boolean = x == y
  def startsWith(x:String, y:String): Boolean = y.startsWith(x)
}

/** make operations available without qualification */
import ScalaOperations._

/**
  * Specifically, our Scala semanticsinterprets
  *   words of non-terminal N as Scala strings
  *   words of non-terminal F as Scala Booleans
  */
object ScalaSemantics {
  /* one translation function per non-terminal */
  def translate_N(x: N): String = {
    x match {
      /* one case per production */
      case Zero() => ""
      case One() => "|"
      case Sum(m,n) =>
        /* one recursive call per constructor argument */
        val mT = translate_N(m)
        val nT = translate_N(n)
        /* compositionally building the result for the case */
        conc(mT,nT)
      case Product(m,n) =>
        val mT = translate_N(m)
        val nT = translate_N(n)
        replace(mT, '|', nT)
    }
  }

  def translate_F(x: F): Boolean = {
    x match {
      case Equality(m,n) =>
        val mT = translate_N(m)
        val nT = translate_N(n)
        equal(mT,nT)
      case Ordering(m,n) =>
        val mT = translate_N(m)
        val nT = translate_N(n)
        startsWith(mT,nT)
    }
  }
}
