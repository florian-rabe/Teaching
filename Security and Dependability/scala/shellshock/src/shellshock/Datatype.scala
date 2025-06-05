package shellshock

// ********************* commands

abstract class Comm

case class Run(shellcall: String) extends Comm

case class Apply(funName: Name, argument: Expr) extends Comm

case class Sequence(first: Comm, second: Comm) extends Comm

// those commands that are definitions 
abstract class DefComm extends Comm 

case class ValDef(name: Name, value: Expr) extends DefComm

case class FunDef(funName: Name, varName: Name, body: Comm) extends DefComm


// ********************* expressions

sealed abstract class Expr

case class NameRef(name: Name) extends Expr

case class StringExpr(value: String) extends Expr

// ********************* names

case class Name(name: String)