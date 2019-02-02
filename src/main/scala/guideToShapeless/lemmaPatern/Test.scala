package guideToShapeless.lemmaPatern

import Penultimate.Ops
import guideToShapeless.IceCream
import guideToShapeless.assertSameValueAndType
import shapeless.::
import shapeless.HNil

object Test {

  def main(args: Array[String]): Unit = {
    type BigList = String :: Int :: Boolean :: Double :: HNil
    val bigList: BigList = "foo" :: 123 :: true :: 456.0 :: HNil

    assertSameValueAndType(true)(Penultimate[BigList].apply(bigList))
    assertSameValueAndType(true)(bigList.penultimate)

    assertSameValueAndType(1)(IceCream("Sundae", 1, false).penultimate)
  }
}
