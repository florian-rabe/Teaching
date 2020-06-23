package wuv.exercise4

/** implementation of a basic typed data language */
object BDL {
  /** types */
  abstract class Type
  case object ItgType extends Type
  case object FltType extends Type
  case object BolType extends Type
  case object StrType extends Type
  case class LstType(elemType: Type) extends Type
  case class RecType(fields: List[(String,Type)]) extends Type

  /** data */
  abstract class Data
  case class Itg(value: Int) extends Data
  case class Flt(value: Double) extends Data
  case class Bol(value: Boolean) extends Data
  case class Str(value: String) extends Data
  case class Lst(tp: Type, elems: List[Data]) extends Data // we take the of the elements in order to be able to infer the type of the empty list
  case class Rec(fields: List[(String,Data)]) extends Data

  /** error thrown during type inference/checking */
  case class TypeMismatch(data: Data, infered: Type, expected: Type) extends Throwable
  /** type inference */
  def inferType(data: Data): Type = data match {
    case Itg(_) => ItgType
    case Flt(_) => FltType
    case Bol(_) => BolType
    case Str(_) => StrType
    case Lst(tp,elems) =>
      elems.foreach {e =>
        val eI = inferType(e)
        if (eI != tp)
          throw TypeMismatch(e,eI,tp)
      }
      tp
    case Rec(fields) =>
      val fieldsI = fields map {case (k,v) =>
        (k,inferType(v))
      }
      RecType(fieldsI)
  }
}

object Coding {
  
}
