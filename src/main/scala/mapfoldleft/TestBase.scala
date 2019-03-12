package mapfoldleft

abstract class TestBase(className: String) {

  def main(args: Array[String]): Unit = {
    testList()
    testMap()
    testSet()
  }

  protected def testList(): Unit
  protected def testMap(): Unit
  protected def testSet(): Unit

  protected def printOK(collectionType: String): Unit =
    println(s"$className $collectionType OK")
}
