package sfol

case class Theory(decls: List[Declaration])

abstract class Declaration

abstract class NamedDeclaration extends Declaration {
    val name: String
}

case class TypeDecl(name: String) extends NamedDeclaration
case class FunDecl(name: String, inputs: List[Type], output: Type) extends NamedDeclaration
case class PredDecl(name: String, inputs: List[Type]) extends NamedDeclaration
case class Axiom(form: Formula) extends Declaration

abstract class Expression

abstract class Type extends Expression
case class TypeRef(y: String) extends Type
abstract BuiltinType extends Type
case class IntType() extends BuiltinType
case class BoolType() extends BuiltinType
case class GradeType() extends BuiltinType
// ...

abstract class Term extends Expression
case class VarRef(x: String) extends Term
case class FunRef(f: String, inputs: List[Term]) extends Term
abstract BuiltinValue extends Term
case class IntValue(v: Int) extends BuiltinValue {
   def toString = v.toString // string representation
}
case class BoolValue(v: Boolean) extends BuiltinValue {
   def toString = if (v) "true" else "false"
}
case class GradeValue(v: Int) extends BuilinValue {
   // over-generating the intended values, type-checker must check that 0 <= v <= 10
   def toString = List("1.0", "1.3", "1.7", "2.0", "2.3", "2.7", "3.0", "3.3", "3.7", "4.0", "5.0")(10-v)
}
// ...




abstract class Formula extends Expression
case class PredRef(p: String, inputs: List[Term]) extends Formula
case class True() extends Formula
case class Equality(tp: Type, left: Term, right: Term) extends Formula
case class And(left: Formula, right: Formula) extends Formula
case class Forall(vr: String, tp: Type, body: Formula) extends Formula


