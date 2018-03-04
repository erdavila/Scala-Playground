package guideToShapeless.counting

import guideToShapeless.IceCream
import guideToShapeless.assertSameValueAndType
import guideToShapeless.sameTypes
import shapeless._
import shapeless.ops.coproduct
import shapeless.ops.hlist
import shapeless.ops.nat

object Nats {
  def main(args: Array[String]): Unit = {
    numbers()
    genericsLengths()
    caseClassLength()
    hlistOps()
  }

  private def numbers(): Unit = {
    type Zero = Nat._0
    type One = Succ[Zero]
    type Two = Succ[One]

    sameTypes[One, Nat._1]()
    sameTypes[Two, Nat._2]()

    val toInt = nat.ToInt[Two]
    assertSameValueAndType(2)(toInt())

    assertSameValueAndType(2)(Nat.toInt[Nat._2])
    assertSameValueAndType(2)(Nat.toInt(Nat._2))
  }

  private def genericsLengths(): Unit = {
    val hlistLength = hlist.Length[String :: Int :: Boolean :: HNil]
    assertSameValueAndType(3)(Nat.toInt[hlistLength.Out])

    val coproductLength = coproduct.Length[Double :+: Char :+: CNil]
    assertSameValueAndType(2)(Nat.toInt[coproductLength.Out])
  }

  private def caseClassLength(): Unit = {
    trait SizeOf[A] {
      def value: Int
    }

    def sizeOf[A](implicit size: SizeOf[A]): Int = size.value

    implicit def genericSizeOf[A, L <: HList, N <: Nat](
      implicit
        generic: Generic.Aux[A, L],
        size: hlist.Length.Aux[L, N],
        sizeToInt: nat.ToInt[N]
    ): SizeOf[A] =
      new SizeOf[A] {
        def value: Int = sizeToInt()
      }

    assertSameValueAndType(3)(sizeOf[IceCream])
  }

  private def hlistOps(): Unit = {
    val hlist = 123 :: "foo" :: true :: 'x' :: HNil

    assertSameValueAndType("foo")(hlist[Nat._1])
    assertSameValueAndType('x')(hlist(Nat._3))

    assertSameValueAndType("foo" :: true :: HNil)(hlist.take(Nat._3).drop(Nat._1))

    assertSameValueAndType(123 :: "bar" :: "baz" :: 'x' :: HNil)(hlist.updatedAt(Nat._1, "bar").updatedAt(Nat._2, "baz"))
  }
}
