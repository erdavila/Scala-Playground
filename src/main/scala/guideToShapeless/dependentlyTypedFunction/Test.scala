package guideToShapeless.dependentlyTypedFunction

import guideToShapeless.assertSameValueAndType
import shapeless.::
import shapeless.HNil

object Test {

  def main(args: Array[String]): Unit = {
    val second1 = Second[String :: Boolean :: Int :: HNil]
    val second2 = Second[String :: Int :: Boolean :: HNil]

    assertSameValueAndType(true)(second1("foo" :: true :: 123 :: HNil))
    assertSameValueAndType(321)(second2("bar" :: 321 :: false :: HNil))
  }
}
