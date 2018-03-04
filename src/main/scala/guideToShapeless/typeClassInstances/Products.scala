package guideToShapeless.typeClassInstances

import guideToShapeless.IceCream

object Products {

  def main(args: Array[String]): Unit = {
    val iceCreams: List[IceCream] = List(
      IceCream("Sundae", 1, false),
      IceCream("Cornetto", 0, true),
      IceCream("Banana Split", 0, false)
    )

    val result = CsvEncoder.writeCsv(iceCreams)
    val expected =
      """
        |Sundae,1,no
        |Cornetto,0,yes
        |Banana Split,0,no
      """.stripMargin.trim()
    assert(result == expected)
  }
}
