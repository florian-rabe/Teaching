package wuv.languages.example

/** a mini-language using non-terminals S (for strings) and B (for Booleans)
  * as an example of a self-contained target language
  */

sealed abstract class S
/** empty string */
case class Empty() extends S
/** prepend a character to a string */
case class Character(c: Char, rest: S) extends S

sealed abstract class B
case class True() extends B
case class False() extends B

/** the basic operations on the syntax needed to define the translation */
object SBOperations {
  /** S is essentially head-tail lists of characters, so conc is just the usual append function */
  def conc(x:S, y: S): S = x match {
    case Empty() => y
    case Character(h,t) => Character(h, conc(t,y))
  }
  /** similarly, replace is as usual for lists */
  def replace(x:S, c: Char, y:S): S = x match {
    case Empty() => Empty()
    case Character(h,t) =>
      if (h == c) conc(y, t) else Character(h, replace(t, c, y))
  }
  /** equality is obtained from Scala equality */
  def equal(x:S, y:S): B =
    if (x == y) True() else False()
  /** startsWith is the prefix test like for lists */
  def startsWith(x:S, y:S): B = (x,y) match {
    case (Character(c,r),Character(d,s)) =>
      if (c == d && startsWith(r,s) == True()) True()
      else False()
    case (_, Empty()) => True()
    case _ => False()
  }
}

/** make all operations available without qualification */
import SBOperations._

/** the translation function */
object SBSemantics {
  def translate_N(x: N): S = {
    x match {
      case Zero() => Empty()
      case One() => Character('|', Empty())
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

  def translate_F(x: F): B = {
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