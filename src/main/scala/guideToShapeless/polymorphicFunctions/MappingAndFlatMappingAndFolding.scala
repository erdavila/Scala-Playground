package guideToShapeless.polymorphicFunctions

import guideToShapeless.assertSameValueAndType
import shapeless._

object MappingAndFlatMappingAndFolding {

  private object sizeOf extends Poly1 {
    implicit val intCase: Case.Aux[Int, Int] =
      at(identity)

    implicit val stringCase: Case.Aux[String, Int] =
      at(_.length)

    implicit val booleanCase: Case.Aux[Boolean, Int] =
      at(bool => if (bool) 1 else 0)
  }

  def main(args: Array[String]): Unit = {
    mapping()
    flatMapping()
    flatMapping2()
    folding()
  }

  private def mapping(): Unit = {
    val result = (10 :: "hello" :: true :: HNil).map(sizeOf)
    assertSameValueAndType(10 :: 5 :: 1 :: HNil)(result)
  }

  private def flatMapping(): Unit = {
    object valueAndSizeOf extends Poly1 {
      implicit val intCase: Case.Aux[Int, Int :: Int :: HNil] =
        at(num => num :: num :: HNil)

      implicit val stringCase: Case.Aux[String, String :: Int :: HNil] =
        at(str => str :: str.length :: HNil)

      implicit val booleanCase: Case.Aux[Boolean, Boolean :: Int :: HNil] =
        at(bool =>  bool :: (if (bool) 1 else 0) :: HNil)
    }

    val result = (10 :: "hello" :: true :: HNil).flatMap(valueAndSizeOf)
    assertSameValueAndType(10 :: 10 :: "hello" :: 5 :: true :: 1 :: HNil)(result)
  }

  private def flatMapping2(): Unit = {
    object valueAndSizeOf extends Poly1 {
      implicit def base[A](implicit cse: sizeOf.Case.Aux[A, Int]): Case.Aux[A, A :: Int :: HNil] =
        at(value => value :: sizeOf(value) :: HNil)
    }

    val result = (10 :: "hello" :: true :: HNil).flatMap(valueAndSizeOf)
    assertSameValueAndType(10 :: 10 :: "hello" :: 5 :: true :: 1 :: HNil)(result)
  }

  private def folding(): Unit = {
    object sum extends Poly2 {
      implicit val intIntCase: Case.Aux[Int, Int, Int] =
        at((a, b) => a + b)

      implicit val intStringCase: Case.Aux[Int, String, Int] =
        at((a, b) => a + b.length)
    }

    val result = (10 :: "hello" :: 100 :: HNil).foldLeft(0)(sum)
    assertSameValueAndType(115)(result)
  }
}
