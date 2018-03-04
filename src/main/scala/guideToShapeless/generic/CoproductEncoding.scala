package guideToShapeless.generic

import guideToShapeless.Circle
import guideToShapeless.Rectangle
import guideToShapeless.Shape
import guideToShapeless.sameTypes
import shapeless.:+:
import shapeless.CNil
import shapeless.Generic
import shapeless.Inl
import shapeless.Inr

object CoproductEncoding {

  private type Repr = Circle :+: Rectangle :+: CNil

  def main(args: Array[String]): Unit = {
    val gen = Generic[Shape]
    sameTypes[gen.Repr, Repr]

    val circle = Circle(1.0)
    val circleRepr = gen.to(circle)
    assert(Inl(circle) == circleRepr)
    assert(circle == gen.from(circleRepr))

    val rectangle = Rectangle(3.0, 4.0)
    val rectangleRepr = gen.to(rectangle)
    assert(Inr(Inl(rectangle)) == rectangleRepr)
    assert(rectangle == gen.from(rectangleRepr))
  }
}
