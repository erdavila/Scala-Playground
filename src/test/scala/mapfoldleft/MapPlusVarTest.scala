package mapfoldleft

import mapfoldleft.MapPlusVar._

object MapPlusVarTest extends TestBase("MapPlusVar") {

  override def testList(): Unit = {
    val input = List(1, 2, 3)
    val initialState = "@"

    val (finalState, output: List[Char]) = input.mapFoldLeft(initialState) { (s, a) =>
      val b = ('0' + a).toChar
      val ss = s ++ a.toString
      (b, ss)
    }

    assert(finalState == "@123")
    assert(output == List('1', '2', '3'))

    printOK("List")
  }

  override def testMap(): Unit = {
    val input = Map('a -> 1, 'b -> 2, 'c -> 3)
    val initialState = "@"

    val (finalState, output: List[Char]) = input.mapFoldLeft(initialState) { case (s, (aKey, aValue)) =>
      val b = ('0' + aValue).toChar
      val ss = s ++ aKey.name ++ aValue.toString
      (b, ss)
    }

    assert(finalState == "@a1b2c3")
    assert(output == List('1', '2', '3'), output)

    printOK("Map")
  }

  override def testSet(): Unit = {
    val input = Set(1, 2, 3)
    val initialState = "@"

    val (finalState, output: Set[Char]) = input.mapFoldLeft(initialState) { (s, a) =>
      val b = ('0' + a).toChar
      val ss = s ++ a.toString
      (b, ss)
    }

    assert(finalState.sorted == "123@")
    assert(finalState == "@123")
    assert(output == Set('1', '2', '3'))

    printOK("Set")
  }
}
