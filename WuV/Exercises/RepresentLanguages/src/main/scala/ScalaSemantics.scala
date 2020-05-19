package wuv.languages.example

object ScalaOperations {
  def conc(x:String, y: String): String = x+y
  def replace(x:String, c: Char, y:String): String = {
    x.transform(i => if (i == c) y else i)
  }
  def equal(x:String, y:String): Boolean = x == y
  def startsWith(x:String, y:String): Boolean = x.startsWith(y)
}

import ScalaOperations._

object ScalaSemantics {
  def translate_N(x: N): String = {
    x match {
      case Zero() => ""
      case One() => "|"
      case Sum(m,n) =>
        val mT = translate_N(m)
        val nT = translate_N(n)
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
