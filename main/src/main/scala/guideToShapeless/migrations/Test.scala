package guideToShapeless.migrations

import Migration.Implicits._
import Migration.Ops
import guideToShapeless.{ IceCream => IceCreamV1 }
import guideToShapeless.assertSameValueAndType

object Test {

  private case class IceCreamV2a(name: String, inCone: Boolean)
  private case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)
  private case class IceCreamV2c(name: String, inCone: Boolean, numCherries: Int, numWaffles: Int)

  def main(args: Array[String]): Unit = {
    val iceCream = IceCreamV1("Sundae", 1, true)

    assertSameValueAndType(IceCreamV2a("Sundae", true)      )(iceCream.migrateTo[IceCreamV2a])
    assertSameValueAndType(IceCreamV2b("Sundae", true, 1)   )(iceCream.migrateTo[IceCreamV2b])
    assertSameValueAndType(IceCreamV2c("Sundae", true, 1, 0))(iceCream.migrateTo[IceCreamV2c])
  }
}
