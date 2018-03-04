package guideToShapeless

import shapeless.Witness
import shapeless.labelled.FieldType
import shapeless.labelled.KeyTag
import shapeless.labelled.field
import shapeless.syntax.singleton._
import shapeless.test._

object TypeTagging {

  def main(args: Array[String]): Unit = {
    val fieldNameWitness = Witness("numCherries")
    type FieldNameType = fieldNameWitness.T

    val someNumber = 123

    val numCherries = "numCherries" ->> someNumber
    val numCherries2 = field[FieldNameType](someNumber)
    sameTyped(numCherries, numCherries2)
    assert(numCherries == 123)

    val fieldType: FieldType[FieldNameType, Int] = numCherries
    typed[KeyTag[FieldNameType, Int]](fieldType)
    typed[                      Int ](fieldType)
    assert(fieldType == 123)

    val fieldName = getFieldName(numCherries)
    typed[String](fieldName)
    assert(fieldName == "numCherries")

    val fieldValue = getFieldValue(numCherries)
    typed[Int](fieldValue)
    assert(fieldValue == 123)
  }

  private def getFieldName[K, V](value: FieldType[K, V])
      (implicit witness: Witness.Aux[K]): K =
    witness.value

  private def getFieldValue[K, V](value: FieldType[K, V]): V =
    value
}
