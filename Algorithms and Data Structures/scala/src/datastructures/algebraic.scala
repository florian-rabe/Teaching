package datastructures

// ********************** binary relations and examples

abstract class Relation[A] {
   def compare(x:A, y:A): Boolean
}

abstract class PreOrder[A] extends Relation[A]

abstract class Order[A] extends PreOrder[A]

abstract class TotalOrder[A] extends Order[A]

abstract class Equivalence[A] extends PreOrder[A]

object IntSmaller extends TotalOrder[Int] {
  def compare(x:Int, y:Int) = x <= y
}

object IntGreater extends TotalOrder[Int] {
  def compare(x:Int, y:Int) = x >= y
}

object Divides extends Order[Int] {
  def compare(x:Int, y:Int) = y % x == 0
}

object Lexicographic extends TotalOrder[String] {
  def compare(x:String, y:String) =
    if (x == "") true
    else if (y == "") false
    else {
      (x(0).toInt < y(0).toInt) ||
      (x(0) == y(0) && compare(x.substring(0), y.substring(0)))
    }
}

// ********************** binary operations

abstract class Operation[A] {
  def op(x:A, y:A): A
}

abstract class Associative[A] extends Operation[A]

abstract class Monoid[A] extends Associative[A] {
  def unit: A
}

object IntPlus extends Monoid[Int] {
  def op(x:Int, y:Int) = x+y
  def unit = 0
}

object IntTimes extends Monoid[Int] {
  def op(x:Int, y:Int) = x*y
  def unit = 1
}

object StringConcat extends Monoid[String] {
  def op(x:String, y:String) = x+y
  def unit = ""
}