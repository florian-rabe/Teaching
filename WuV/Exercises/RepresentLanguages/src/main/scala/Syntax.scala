package wuv.languages.example

/* one type (= abstract class) per non-terminal */
/* 'sealed' tells Scala that no other subclasses exist */
sealed abstract class N

/* one constructor (= abstract class) per production */
/* we need to invent names for the productions, (here 'Zero') */
case class Zero() extends N {
  /* concrete syntax only appears via a parser (omitted here) and serializer ('toString') */
  /* Scala auto-generates toString methods, so we need to 'override' */
  override def toString = "0"
}
case class One() extends N {
  override def toString = "1"
}

case class Literal(value: Int) extends N {
  override def toString = value.toString
}

/* one constructor argument per occurrence of a non-terminal on the right-hand side of the production */
/* we need to invent names for the constructor arguments (here 'left', 'right') */
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
