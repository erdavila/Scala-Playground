package guideToShapeless.polymorphicFunctions

import guideToShapeless.assertSameValueAndType
import shapeless._

object PolymorphicFunctions {

  def main(args: Array[String]): Unit = {
    testMyPoly()
    testMultiply()
    testTotal()
  }

  private def testMyPoly(): Unit = {
    object myPoly extends Poly1 {
      implicit val intCase: Case.Aux[Int, Double] =
        at(num => num / 2.0)

      implicit val stringCase: Case.Aux[String, Int] =
        at(str => str.length)
    }

    val d = myPoly(123)
    assertSameValueAndType(61.5)(d)
    assertSameValueAndType(61.5)(myPoly[Int](123))

    val i = myPoly("hello")
    assertSameValueAndType(5)(i)
    assertSameValueAndType(5)(myPoly[String]("hello"))
  }

  private def testMultiply(): Unit = {
    object multiply extends Poly2 {
      implicit val intIntCase: Case.Aux[Int, Int, Int] =
        at((a, b) => a * b)

      implicit val intStrCase: Case.Aux[Int, String, String] =
        at((a, b) => b.toString * a)
    }

    assertSameValueAndType(12)(multiply(3, 4))

    assertSameValueAndType("444")(multiply(3, "4"))
  }

  private def testTotal(): Unit = {
    object total extends Poly1 {
      implicit def base[A](implicit num: Numeric[A]): Case.Aux[A, Double] =
        at(num.toDouble)

      implicit def option[A](implicit num: Numeric[A]): Case.Aux[Option[A], Double] =
        at(opt => opt.map(num.toDouble).getOrElse(0.0))

      implicit def list[A](implicit num: Numeric[A]): Case.Aux[List[A], Double] =
        at(list => num.toDouble(list.sum))
    }

    val i = total(10)
    assertSameValueAndType(10.0)(i)

    val o = total(Option(20.0))
    assertSameValueAndType(20.0)(o)

    val l = total(List(1L, 2L, 3L))
    assertSameValueAndType(6.0)(l)
  }
}
