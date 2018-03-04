package guideToShapeless.generic

import guideToShapeless.IceCream
import guideToShapeless.assertSameValueAndType
import guideToShapeless.sameTypes
import shapeless.::
import shapeless.Generic
import shapeless.HNil
import shapeless.test.typed

object ProductEncoding {

  private type Repr = String :: Int :: Boolean :: HNil
  
  def main(args: Array[String]): Unit = {
    val iceCreamGen = Generic[IceCream]
    sameTypes[iceCreamGen.Repr, Repr]()

    val iceCream = IceCream("Sundae", 1, false)
    val repr = iceCreamGen.to(iceCream)
    val iceCream2 = iceCreamGen.from(repr)

    typed[Repr](repr)
    assertSameValueAndType(iceCream)(iceCream2)
  }
}
