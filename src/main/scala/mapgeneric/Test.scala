package mapgeneric

object Test {

  private case class Person(name: String, age: Option[Int])

  def main(args: Array[String]): Unit = {
    toMap()
    fromMap()
  }

  private def toMap(): Unit = {
    def test(person: Person, expected: Map[String, Any]): Unit = {
      val map = MapGeneric[Person].toMap(person)
      assert(map == expected, map)
    }

    test( // complete
      Person("John", Some(37)),
      Map("name" -> "John", "age" -> 37)
    )

    test( // no age
      Person("John", None),
      Map("name" -> "John")
    )

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
      Some(Person("John", Some(37))),
    )

    testNone( // no name
      Map("age" -> 37)
    )

    testNone( // wrongly typed name
      Map("name" -> 'John, "age" -> 37)
    )

    test( // no age
      Map("name" -> "John"),
      Some(Person("John", None)),
    )

    testNone( // wrongly typed age
      Map("name" -> "John", "age" -> "37")
    )

    println("fromMap OK")
  }
}
