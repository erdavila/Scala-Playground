package guideToShapeless

import shapeless.LabelledGeneric
import shapeless.record._

object RecordOps {

  private case class IceCreamNoCone(name: String, numCherries: Int)

  def main(args: Array[String]): Unit = {
    val iceCream = IceCream("Sundae", 1, false)
    val iceCreamGen = LabelledGeneric[IceCream]
    val sundae = iceCreamGen.to(iceCream)

    assertSameValueAndType("Sundae")(sundae.get('name))
    assertSameValueAndType(1       )(sundae.get('numCherries))

    assertSameValueAndType(IceCream("Sundae", 3, false))(iceCreamGen.from(sundae.updated('numCherries, 3)))

    val iceCreamNoConeGen = LabelledGeneric[IceCreamNoCone]
    val (removedValue: Boolean, iceCreamNoCone) = sundae.remove('inCone)
    assert(removedValue == false)
    assertSameValueAndType(IceCreamNoCone("Sundae", 1))(iceCreamNoConeGen.from(iceCreamNoCone))

    assertSameValueAndType(IceCream("MASSIVE Sundae", 1, false))(iceCreamGen.from(sundae.updateWith('name)("MASSIVE " + _)))

    val expectedMap = Map(
      'name -> "Sundae",
      'numCherries -> 1,
      'inCone -> false
    )
    assertSameValueAndType(expectedMap)(sundae.toMap)
  }
}
