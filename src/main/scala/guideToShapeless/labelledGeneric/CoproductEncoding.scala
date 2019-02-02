package guideToShapeless.labelledGeneric

import guideToShapeless.Circle
import guideToShapeless.Rectangle
import guideToShapeless.Shape
import guideToShapeless.typeClassInstances.JsonEncoder
import guideToShapeless.typeClassInstances.JsonNumber
import guideToShapeless.typeClassInstances.JsonObject
import shapeless.:+:
import shapeless.LabelledGeneric
import shapeless.Witness
import shapeless.labelled.FieldType
import shapeless.tag.Tagged
import shapeless.test.typed
import shapeless.CNil

object CoproductEncoding {

  def main(args: Array[String]): Unit = {
    val circle = Circle(1.0)

    val shapeGen = LabelledGeneric[Shape]
    val gen = shapeGen.to(circle)

    val circleWitness = Witness("Circle")
    val rectangleWitness = Witness("Rectangle")
    type T =
      FieldType[Symbol with Tagged[   circleWitness.T], Circle] :+:
      FieldType[Symbol with Tagged[rectangleWitness.T], Rectangle] :+:
      CNil
    typed[T](gen)

    val encoded = JsonEncoder[Shape].encode(circle)
    val expected =
      JsonObject(
        List(
          "Circle" -> JsonObject(
            List(
              "radius" -> JsonNumber(1.0)
            )
          )
        )
      )
    assert(encoded == expected)
  }
}
