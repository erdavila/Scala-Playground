package guideToShapeless.labelledGeneric

import guideToShapeless.IceCream
import guideToShapeless.sameTypes
import guideToShapeless.typeClassInstances.JsonBoolean
import guideToShapeless.typeClassInstances.JsonEncoder
import guideToShapeless.typeClassInstances.JsonNumber
import guideToShapeless.typeClassInstances.JsonObject
import guideToShapeless.typeClassInstances.JsonString
import shapeless.::
import shapeless.HNil
import shapeless.LabelledGeneric
import shapeless.Witness
import shapeless.labelled.FieldType
import shapeless.tag.Tagged
import shapeless.test.typed

object ProductEncoding {

  def main(args: Array[String]): Unit = {
    val iceCream = IceCream("Sundae", 1, false)

    val iceCreamGen = LabelledGeneric[IceCream]
    val gen = iceCreamGen.to(iceCream)

    val nameWitness = Witness("name")
    val numCherriesWitness = Witness("numCherries")
    val inConeWitness = Witness("inCone")
    type T =
      FieldType[Symbol with Tagged[       nameWitness.T], String] ::
      FieldType[Symbol with Tagged[numCherriesWitness.T], Int] ::
      FieldType[Symbol with Tagged[     inConeWitness.T], Boolean] ::
      HNil
    typed[T](gen)
    sameTypes[T, iceCreamGen.Repr]()

    val encoded = JsonEncoder[IceCream].encode(iceCream)
    val expected =
      JsonObject(
        List(
          "name" -> JsonString("Sundae"),
          "numCherries" -> JsonNumber(1),
          "inCone" -> JsonBoolean(false)
        )
      )
    assert(encoded == expected)
  }
}
