package guideToShapeless.typeClassInstances

import guideToShapeless.Circle
import guideToShapeless.Rectangle
import guideToShapeless.Shape

object Coproducts {

  def main(args: Array[String]): Unit = {
    val shapes: List[Shape] = List(
      Rectangle(3.0, 4.0),
      Circle(1.0)
    )

    val result = CsvEncoder.writeCsv(shapes)
    val expected =
      """
        |3.0,4.0
        |1.0
      """.stripMargin.trim()
    assert(result == expected)
  }
}
