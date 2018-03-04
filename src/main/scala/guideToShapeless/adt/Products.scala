package guideToShapeless.adt

import guideToShapeless.assertSameValueAndType
import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.test.typed

object Products {

  def main(args: Array[String]): Unit = {
    val product: String :: Int :: Boolean :: HNil =
      "Sundae" :: 1 :: false :: HNil

    typed[HList](product)

    assertSameValueAndType("Sundae")(product.head)
    assertSameValueAndType(1       )(product.tail.head)
    assertSameValueAndType(false   )(product.tail.tail.head)
  }
}
