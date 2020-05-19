package wuv.languages.example

sealed abstract class N

case class Zero() extends N {
  override def toString = "0"
}
case class One() extends N {
  override def toString = "1"
}
case class Sum(left: N, right: N) extends N {
  override def toString = left.toString + "+" + right.toString
}
case class Product(left: N, right: N) extends N {
  override def toString = left.toString + "*" + right.toString
}

sealed abstract class F

case class Equality(left: N, right: N) extends F {
  override def toString = left.toString + "=" + right.toString
}
case class Ordering(left: N, right: N) extends F {
  override def toString = left.toString + "<=" + right.toString
}
