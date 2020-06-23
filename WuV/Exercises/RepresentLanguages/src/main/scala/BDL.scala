package wuv.exercise4

/** implementation of a basic typed data language */
object BDL {
  /** types */
  abstract class Type
  abstract class BaseType extends Type
  case object ItgType extends BaseType
  case object FltType extends BaseType
  case object BolType extends BaseType
  case object StrType extends BaseType
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

import BDL._

case class IllFormedCode(code: String, expectedType: Type) extends Throwable

object AbstractCodecs {
  type Code = String

  abstract class Codec(val tp: Type) {
    def encode(data: Data): Code

    /** partial: may thrown IllFormedCode */
    def decode(code: Code): Data
  }

  abstract class LstCodecOperator {
    def makeCodec(elemCodec: Codec): Codec
  }

  abstract class RecCodecOperator {
    def makeCodec(fieldCodecs: List[Codec]): Codec
  }
}

import AbstractCodecs._

/** boilerplate code for storing available codecs, choosing a codec, and calling encode/decode  */
object Coding {
  /** error thrown during encoding */
  case class NoCodecFound(tp: Type) extends Throwable

  /* register the known codecs here */
  val baseCodecs : List[Codec] = Nil
  val lstCodecOp : Option[LstCodecOperator] = None
  val recCodecOp : Option[RecCodecOperator] = None

  def chooseCodec(tp: Type): Codec = tp match {
    case bt: BaseType => baseCodecs.find(c => c.tp == tp) match {
      case Some(c) => c
      case None => throw NoCodecFound(tp)
    }
    case LstType(t) =>
      val elemCodec = chooseCodec(t)
      val op = lstCodecOp.getOrElse(throw NoCodecFound(tp))
      op.makeCodec(elemCodec)
    case RecType(fs) =>
      val fieldCodecs = fs.map {case (k,t) => chooseCodec(t)}
      val op = recCodecOp.getOrElse(throw NoCodecFound(tp))
      op.makeCodec(fieldCodecs)
  }

  def encode(data: Data): Code = {
    val tp = inferType(data)
    val codec = chooseCodec(tp)
    codec.encode(data)
  }

  def decode(code: Code, tp: Type): Data = {
    val codec = chooseCodec(tp)
    codec.decode(code)
  }
}
