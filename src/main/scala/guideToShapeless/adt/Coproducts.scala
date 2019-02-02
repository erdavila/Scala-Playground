package guideToShapeless.adt

import guideToShapeless.Circle
import guideToShapeless.Rectangle
import shapeless.:+:
import shapeless.CNil
import shapeless.Coproduct
import shapeless.Inl
import shapeless.Inr
import shapeless.test.typed

object Coproducts {

  private final case class Square(side: Double)
  private type Shape = Rectangle :+: Circle :+: Square :+: CNil

  def main(args: Array[String]): Unit = {
    val rectangle: Shape =         Inl(Rectangle(3.0, 4.0))
    val circle   : Shape =     Inr(Inl(Circle(1.0)))
    val square   : Shape = Inr(Inr(Inl(Square(2.0))))

    typed[Coproduct](rectangle)
    typed[Coproduct](circle)
    typed[Coproduct](square)
  }
}
