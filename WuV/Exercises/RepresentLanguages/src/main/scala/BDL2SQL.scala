package wuv.bdl

import BDL._

object BDL2SQL {
  /** maps a vocabulary to a list of TABLE statements */
  def exportVocabulary(voc: Vocabulary) = {
    val vocF = flatten(voc)
    val declsS = vocF.decls.map{d => exportDeclaration(voc.getBefore(d.name), d)}
    declsS.mkString("\n")
  }

  /** SQL cannot handle nested ADTs.
    * Instead, we eliminate them non-compositionally.
    * Instead, for every ADTElementDefinition with name n, for every field with name f whose type is a nested ADT,
    * we generate a new ADTElementDefinition with name n.f, and replace the field value with a reference to it.
    * That way only DataRef and not ADTElement occur as field values in ADTElementDefinition.
    */
  def flatten(voc: Vocabulary) = {
    val declsFlat = voc.decls.flatMap {
      case tpDef: ADTDefinition => List(tpDef)
      case elemDef: ADTElementDefinition =>
        val elemValue = elemDef.value
        var result: List[ADTElementDefinition] = Nil
        val newFields = elemValue.fields.map {case (name,value) =>
          value match {
            case ae: ADTElement =>
              val newName = elemDef.name + "." + name
              result ::= ADTElementDefinition(newName,ae)
              (name,DataRef(newName))
            case d =>
              (name,value)
          }
        }
        val newElemValue = ADTElement(elemValue.tp,newFields)
        result ::= = ADTElementDefinition(elemDef.name,newElemValue)
        result.reverse
    }
    Vocabulary(declsFlat)
  }

  /** maps an ADT definition to a TABLE statement, datum definitions to INSERT statements */
  def exportDeclaration(voc: Vocabulary, decl: Declaration) = decl match {
    case ADTDefinition(name, fields) =>
      val fieldsS = fields map {case ADTField(n,tp,_) =>
        // non-compositional step: a nested case distinction
        val tpS = tp match {
          // translate nested ADT fields to foreign keys
          case ADTRef(_) => "ID"
          // encode everything else as strings
          case _ => "string"
        }
        n + " " + tpS
      }
      val idField = "id ID" // add a column for ids
      "TABLE (" + idField + fieldsS.mkString(",") + ")"
    case ADTElementDefinition(d, ADTElement(t,fields)) =>
      val adt = voc.getADT(t)
      val fieldNames = fields.map(_._1)
      val fieldCodes = fields.map {f =>
        val (name,value) = f
        // references to named ADT elements become ids, everything else is encoded
        val code = value match {
          case DataRef(n) => n
          case _ =>
            val codec = adt.get(name).codec
            Coding.encode(value, codec)
        }
      }
      "INSERT INTO" + t + "(" + "id" + fieldNames.mkString(",") + ")" +
      "VALUES (" + d + ", " + fieldCodes.mkString(",") + ")"
  }

  /** maps an ADT element to a CSV object of encoded field values */
  def exportData(voc: Vocabulary, elem: ADTElement) = {
    val adt = voc.getADT(elem.tp)
    val encodedFields = adt.fields.map {f =>
      val datum = elem.get(f.name).get // field must exist and must have type f.tp if elem is well-formed
      Coding.encode(datum, f.codec)
    }
    Escaping.merge(encodedFields)
  }

  /** reads ADT data from its JSON encoding */
  def importData(voc: Vocabulary, elemCoded: String, adtName: ID) = {
    val adt = voc.getADT(adtName)
    val codes = Escaping.split(elemCoded)
    val decodedFields = (codes zip adt.fields).map {case (c,f) =>
      val data = Coding.decode(c, f.codec)
      (f.name, data)
    }
    ADTElement(adtName, decodedFields)
  }
}

/** helper object for merging and splitting multiple values (which requires some escaping)
  * for now, we use a naive implementation that splits at whitespace, i.e., we assume no value contains whitespace */
object Escaping {
  def merge(values: List[String]) = {
    values.mkString(" ")
  }
  def split(l: String): List[String] = {
    l.split("\\s").toList
  }
}