package wuv.bdl

/** implementation of a basic typed data language */
object BDL {
  /** identifiers */
  type ID = String

  /** vocabularies */
  case class Vocabulary(decls: List[Declaration]) {
    /** helper function to retrieve the context of a declaration, i.e., everything before it */
    def getBefore(name: ID) = {
      val ds = decls.takeWhile(_.name != name)
      Vocabulary(ds)
    }
    /** helper function to retrieve the definition of an ADT (must exist) */
    def getADT(name: ID) = decls.collectFirst {
      case d: ADTDefinition if d.name == name => d
    }.get
  }

  /** declarations */
  abstract class Declaration {
    // every declaration must have a name
    def name: ID
  }
  case class ADTDefinition(name: ID, fields: List[ADTField]) extends Declaration {
    /** helper function to retrieve a field */
    def get(name: ID) = fields.find(_.name == name).get
  }
  case class ADTElementDefinition(name: ID, value: ADTElement) extends Declaration

  /** helper class for ADTDefinition */
  case class ADTField(name: ID, tp: Type, codec: CodecExpression)
  /** annotation of ADTField that gives the codec that should be used for the value of the field */
  abstract class CodecExpression
  case class BaseCodec(name: ID) extends CodecExpression
  case class ListCodec(name: ID, elemCodec: CodecExpression) extends CodecExpression
  case class RecordCodec(name: ID, fieldCodecs: List[(ID,CodecExpression)]) extends CodecExpression

  /** types */
  abstract class Type
  case class ADTRef(name: ID) extends Type
  abstract class BaseType extends Type
  case object ItgType extends BaseType
  case object FltType extends BaseType
  case object BolType extends BaseType
  case object StrType extends BaseType
  case class LstType(elemType: Type) extends Type
  case class RecType(fields: List[(ID,Type)]) extends Type

  case object SemesterType extends BaseType

  /** data */
  abstract class Data
  case class DataRef(name: ID) extends Data
  case class Itg(value: Int) extends Data
  case class Flt(value: Double) extends Data
  case class Bol(value: Boolean) extends Data
  case class Str(value: String) extends Data
  case class Lst(tp: Type, elems: List[Data]) extends Data // we take the of the elements in order to be able to infer the type of the empty list
  case class Rec(fields: List[(ID,Data)]) extends Data
  case class ADTElement(tp: ID, fields: List[(ID,Data)]) extends Data {
    /** helper function to retrieve the value of a field */
    def get(field: ID) = fields.find(_._1 == field).map(_._2)
  }

  case class Semester(year: Int, summer: Boolean) extends Data

  /** error thrown during type inference/checking */
  case class TypeMismatch(data: Data, infered: Type, expected: Type) extends Throwable
  case class TypeError(data: Data, message: String) extends Throwable
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
      LstType(tp)
    case Rec(fields) =>
      val fieldsI = fields map {case (k,v) =>
        (k,inferType(v))
      }
      val keys = fields.map(_._1)
      if (keys.distinct != keys)
        throw TypeError(data, "keys not distinct")
      RecType(fieldsI)
    case ADTElement(t,_) => ADTRef(t) // TODO check fields against type definition
    case Semester(_,_) => SemesterType
  }
}

import BDL._
import ConcreteCodecs._

case class IllFormedCode(code: String, expectedType: Type) extends Throwable

object AbstractCodecs {
  type Code = String

  abstract class Codec(val tp: Type, val expr: CodecExpression) {
    /** precondition: data has type tp */
    def encode(data: Data): Code

    /** partial: may thrown IllFormedCode */
    def decode(code: Code): Data
  }

  abstract class LstCodecOperator(val id: ID) {
    def makeCodec(elemCodec: Codec): Codec
  }

  abstract class RecCodecOperator(val id: ID) {
    def makeCodec(fieldCodecs: List[(ID,Codec)]): Codec
  }
}

import AbstractCodecs._

object ConcreteCodecs {
  /** example codec for integers */
  object StandardInt extends Codec(ItgType, BaseCodec("StandardInt")) {
    def encode(data: Data) = {
      val Itg(i) = data
      i.toString
    }
    def decode(c: Code) = {
      val i = try {c.toInt}
      catch {case _: Exception => throw IllFormedCode(c, tp)}
      Itg(i)
    }
  }

  /** example codec for semesters as standardized in the lecture */
  object StandardSemester extends Codec(SemesterType, BaseCodec("StandardSemester")) {
    def encode(data: Data) = {
      val Semester(y,s) = data
      "\"" + (if (s) "SS" else "WS")  + y.toString + "\""
    }
    def decode(c: Code) = {
      if (!c.endsWith("\""))
        throw IllFormedCode(c,tp)
      val s = if (c.startsWith("\"SS")) true
        else if (c.startsWith("\"WS")) false
        else throw IllFormedCode(c,tp)
      val rest = c.substring(3,c.length-1)
      val y = try {rest.toInt}
      catch {case _: Exception => throw IllFormedCode(c, tp)}
      Semester(y,s)
    }
  }

  object CommaSeparatedListCodec extends LstCodecOperator("CommaSeparatedList") {
    def makeCodec(elemCodec: Codec): Codec = new Codec(LstType(elemCodec.tp), ListCodec(id, elemCodec.expr)) {
      def encode(data: Data): Code = {
        val Lst(_, elems) = data
        val elemCodes = elems map {ec => elemCodec.encode(ec)}
        "[" + elemCodes.mkString(",") + "]"
      }
      def decode(code: Code): Data = {
        val seeker = new SeparatorSeeker(code)
        var elems : List[Data] = Nil
        var i = 0
        i = seeker.seekChar(i,'[')
        if (i == -1) throw IllFormedCode(code, tp)
        i = i + 1
        val next = seeker.seekNonWhitespace(i)
        // check for empty list []
        if (next != -1 && code(next) == ']') return Lst(elemCodec.tp, Nil)
        // now we always expect one element
        while (true) {
          val endOfNextElem = seeker.seekCommaOrSquareClose(i)
          if (endOfNextElem == -1)
            throw IllFormedCode(code,tp)
          val sep = code(endOfNextElem)
          val elemCode = code.substring(i,endOfNextElem)
          val elem = elemCodec.decode(elemCode)
          elems = elems ::: List(elem)
          i = endOfNextElem + 1
          if (sep == ']') {
            // end of list, return list if no extraneous characters
            if (seeker.seekNonWhitespace(i) != -1)
              throw IllFormedCode(code,tp)
            else
              return Lst(elemCodec.tp, elems)
          } else if (sep == ',') {
            // continue
          } else {
            // impossible
          }
        }
        null // impossible
      }
    }
  }
}

import ConcreteCodecs._

/** boilerplate code for storing available codecs, choosing a codec, and calling encode/decode  */
object Coding {
  /** error thrown during encoding */
  case class NoCodecFoundForType(tp: Type) extends Throwable
  case class NoCodecFoundForID(id: ID) extends Throwable

  /* register the known codecs here */
  val baseCodecs : List[Codec] = List(StandardInt, StandardSemester)
  val lstCodecOp : List[LstCodecOperator] = List(CommaSeparatedListCodec)
  val recCodecOp : List[RecCodecOperator] = Nil

  /** builds a codec for a given codec expressions; there may be multiple codecs for the same type, which are distinguished by their codec expression */
  def buildCodec(ce: CodecExpression): Codec = ce match {
    case BaseCodec(id) => baseCodecs.find(c => c.expr == BaseCodec(id)).getOrElse(throw NoCodecFoundForID(id))
    case ListCodec(id,elemCE) =>
      val listCodec = lstCodecOp.find(_.id == id).getOrElse(throw NoCodecFoundForID(id))
      val elemCodec = buildCodec(elemCE)
      listCodec.makeCodec(elemCodec)
    case RecordCodec(id,fieldCodecExprs) =>
      val recCodec = recCodecOp.find(_.id == id).getOrElse(throw NoCodecFoundForID(id))
      val fieldCodecs = fieldCodecExprs.map {case (k,ce) => (k,buildCodec(ce))}
      recCodec.makeCodec(fieldCodecs)
  }

  /** builds a default codec for a given type by choosing the first known codec for the needed type */
  def chooseCodec(tp: Type): Codec = tp match {
    case bt: BaseType => baseCodecs.find(c => c.tp == tp) match {
      case Some(c) => c
      case None => throw NoCodecFoundForType(tp)
    }
    case LstType(t) =>
      val elemCodec = chooseCodec(t)
      val op = lstCodecOp.headOption.getOrElse(throw NoCodecFoundForType(tp))
      op.makeCodec(elemCodec)
    case RecType(fs) =>
      val fieldCodecs = fs.map {case (k,t) => (k,chooseCodec(t))}
      val op = recCodecOp.headOption.getOrElse(throw NoCodecFoundForType(tp))
      op.makeCodec(fieldCodecs)
  }

  /** encodes using a particular codec */
  def encode(data: Data, ce: CodecExpression): Code = {
    val codec = buildCodec(ce)
    codec.encode(data)
  }

  /** encodes using a default codec obtained from the type */
  def encodeDefault(data: Data): Code = {
    val tp = inferType(data)
    val codec = chooseCodec(tp)
    codec.encode(data)
  }

  /** decodes using a particular codec */
  def decode(code: Code, ce: CodecExpression): Data = {
    val codec = buildCodec(ce)
    codec.decode(code)
  }

  /** decodes using default codec */
  def decode(code: Code, tp: Type): Data = {
    val codec = chooseCodec(tp)
    codec.decode(code)
  }
}

/** helper class to seek a character in a string while skipping over bracketed structures
  * we assume the only bracketing introduced by any codec are [...], {...}, "..."
  */
private class SeparatorSeeker(s: String) {
  val length = s.length

  def seekChar(from: Int, c: Char) = seek(from, _ == c, Nil)

  def seekSquareClose(from: Int) = seek(from, _ == ']', List('{','"'))
  def seekCurlyClose(from: Int) = seek(from, _ == '}', List('[','"'))
  def seekQuote(from: Int) = seek(from, _ == '"', List('{','['))
  def seekCommaOrSquareClose(from: Int) = seek(from, c => c == ',' || c == ']', List('{','[','"'))

  def seekNonWhitespace(from: Int) = seek(from, c => !c.isWhitespace, Nil)

  private def seek(from: Int, cond: Char => Boolean, skip: List[Char]): Int = {
    var i = from
    if (i < 0 || i >= length) return -1
    while (i < length) {
      s(i) match {
        // return index of seeked character
        case c if cond(c) => return i
        // skip over opening brackets
        case c if skip.contains(c) => i = i+1
        // if an open bracket is found, we seek for the corresponding closing bracket, skipping all other open brackets
        case '[' => i = seekSquareClose(i)
        case '{' => i = seekCurlyClose(i)
        case '"' => i = seekQuote(i)
        // skip over current character
        case _ => i = i + 1
      }
    }
    return -1
  }
}

object Test {
  def main(args: Array[String]) {
    val data = Lst(SemesterType, List(Semester(2020, true), Semester(2020, false)))
    println("data:    " + data.toString)
    val tp = inferType(data)
    println("type:    " + tp.toString)
    val codec = Coding.chooseCodec(tp)
    val code = codec.encode(data) // """["SS2020", "WS2020"]"""
    println("encoded: " + code)
    val dec = codec.decode(code)
    println("decoded: " + dec.toString)
    println("equal:   " + (dec == data))
  }
}
