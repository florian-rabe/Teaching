package wuv.bdl

import BDL._

object BDL2SQL {
  /** maps a vocabulary to a list of TABLE statements */
  def exportVocabulary(voc: Vocabulary) = {
    voc.decls.map(d => exportDeclaration(d)).mkString("\n")

  }
  /** maps an ADT definition to a TABLE statement, datum definitions are skipped */
  def exportDeclaration(decl: Declaration) = decl match {
    case ADTDefinition(name, fields) =>
      val fieldsS = fields map {case ADTField(n,_,_) =>
        n + " string"
      }
      val s = "TABLE (" + fieldsS.mkString(",") + ")"
      List(s)
    case _:DatumDefinition => Nil
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