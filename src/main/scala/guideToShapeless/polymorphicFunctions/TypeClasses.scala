package guideToShapeless.polymorphicFunctions

import guideToShapeless.{ IceCream => IceCream1 }
import guideToShapeless.assertSameValueAndType
import shapeless._
import shapeless.ops.hlist

private trait ProductMapper[A, B, P] {
  def apply(a: A): B
}

private object ProductMapper {
  implicit class ProductMapperOps[A](a: A) {
    class Builder[B] {
      def apply[P <: Poly](poly: P)(implicit pm: ProductMapper[A, B, P]): B =
        pm.apply(a)
    }

    def mapTo[B]: Builder[B] = new Builder[B]
  }

  implicit def genericProductMapper[
    A, B,
    P <: Poly,
    ARepr <: HList,
    BRepr <: HList
  ](
    implicit
      aGen: Generic.Aux[A, ARepr],
      bGen: Generic.Aux[B, BRepr],
      mapper: hlist.Mapper.Aux[P, ARepr, BRepr]
  ): ProductMapper[A, B, P] =
    new ProductMapper[A, B, P] {
      def apply(a: A): B =
        bGen.from(mapper.apply(aGen.to(a)))
    }
}

object TypeClasses {

  import ProductMapper.ProductMapperOps

  private object conversions extends Poly1 {
    implicit val intCase:  Case.Aux[Int, Boolean] = at(_ > 0)
    implicit val boolCase: Case.Aux[Boolean, Int] = at(if (_) 1 else 0)
    implicit val strCase:  Case.Aux[String, String] = at(identity)
  }

  private case class IceCream2(name: String, hasCherries: Boolean, numCones: Int)

  def main(args: Array[String]): Unit = {
    val result = IceCream1("Sundae", 1, false).mapTo[IceCream2](conversions)
    val expected = IceCream2("Sundae", true, 0)
    assertSameValueAndType(expected)(result)
  }
}
