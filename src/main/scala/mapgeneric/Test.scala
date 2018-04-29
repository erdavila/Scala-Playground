package mapgeneric

object Test {

  private case class Person(name: String, age: Int)

  def main(args: Array[String]): Unit = {
    toMap()
    fromMap()
  }

  private def toMap(): Unit = {
    val person = Person("John", 37)
    val map = MapGeneric[Person].toMap(person)
    val expected = Map("name" -> "John", "age" -> 37)
    assert(map == expected, map)

    println("toMap OK")
  }

  private def fromMap(): Unit = {
    def test(map: Map[String, Any], expected: Option[Person]): Unit = {
      val person = MapGeneric[Person].fromMap(map)
      assert(person == expected, person)
    }

    def testNone(map: Map[String, Any]): Unit =
      test(map, None)

    test( // complete
      Map("name" -> "John", "age" -> 37),
      Some(Person("John", 37)),
    )

    testNone( // no name
      Map("age" -> 37)
    )

    testNone( // wrongly typed name
      Map("name" -> 'John, "age" -> 37)
    )

    testNone( // no age
      Map("name" -> "John")
    )

    testNone( // wrongly typed age
      Map("name" -> "John", "age" -> "37")
    )

    println("fromMap OK")
  }
}
