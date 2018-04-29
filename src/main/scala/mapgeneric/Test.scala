package mapgeneric

object Test {

  private case class Person(name: String, age: Option[Int], homeAddress: Address, workAddress: Option[Address])
  private case class Address(street: String, number: Int)

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
      Person("John", Some(37), Address("Some st", 2018), Some(Address("Other st", 12345))),
      Map(
        "name" -> "John", "age" -> 37,
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
        "workAddress" -> Map("street" -> "Other st", "number" -> 12345),
      )
    )

    test( // no age
      Person("John", None, Address("Some st", 2018), Some(Address("Other st", 12345))),
      Map(
        "name" -> "John",
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
        "workAddress" -> Map("street" -> "Other st", "number" -> 12345),
      )
    )

    test( // no work address
      Person("John", Some(37), Address("Some st", 2018), None),
      Map(
        "name" -> "John", "age" -> 37,
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
      )
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
      Map(
        "name" -> "John", "age" -> 37,
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
        "workAddress" -> Map("street" -> "Other st", "number" -> 12345),
      ),
      Some(Person("John", Some(37), Address("Some st", 2018), Some(Address("Other st", 12345)))),
    )

    testNone( // no name
      Map(
        "age" -> 37,
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
        "workAddress" -> Map("street" -> "Other st", "number" -> 12345),
      )
    )

    testNone( // wrongly typed name
      Map(
        "name" -> 'John, "age" -> 37,
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
        "workAddress" -> Map("street" -> "Other st", "number" -> 12345),
      )
    )

    test( // no age
      Map(
        "name" -> "John",
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
        "workAddress" -> Map("street" -> "Other st", "number" -> 12345),
      ),
      Some(Person("John", None, Address("Some st", 2018), Some(Address("Other st", 12345)))),
    )

    testNone( // wrongly typed age
      Map(
        "name" -> "John", "age" -> "37",
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
        "workAddress" -> Map("street" -> "Other st", "number" -> 12345),
      )
    )

    testNone( // no home address
      Map(
        "name" -> "John", "age" -> 37,
        "workAddress" -> Map("street" -> "Other st", "number" -> 12345),
      )
    )

    testNone( // wrongly typed home address
      Map(
        "name" -> "John", "age" -> 37,
        "homeAddress" -> "Some st, 2018",
        "workAddress" -> Map("street" -> "Other st", "number" -> 12345),
      )
    )

    testNone( // no home address number
      Map(
        "name" -> "John", "age" -> 37,
        "homeAddress" -> Map("street" -> "Some st"),
        "workAddress" -> Map("street" -> "Other st", "number" -> 12345),
      )
    )

    testNone( // wrongly typed home address number
      Map(
        "name" -> "John", "age" -> 37,
        "homeAddress" -> Map("street" -> "Some st", "number" -> "2018"),
        "workAddress" -> Map("street" -> "Other st", "number" -> 12345),
      )
    )

    test( // no work address
      Map(
        "name" -> "John", "age" -> 37,
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
      ),
      Some(Person("John", Some(37), Address("Some st", 2018), None)),
    )

    testNone( // wrongly typed work address
      Map(
        "name" -> "John", "age" -> 37,
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
        "workAddress" -> "Other st, 12345",
      )
    )

    testNone( // no work address number
      Map(
        "name" -> "John", "age" -> 37,
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
        "workAddress" -> Map("street" -> "Other st"),
      )
    )

    testNone( // wrongly typed work address number
      Map(
        "name" -> "John", "age" -> 37,
        "homeAddress" -> Map("street" -> "Some st", "number" -> 2018),
        "workAddress" -> Map("street" -> "Other st", "number" -> "12345"),
      )
    )

    println("fromMap OK")
  }
}
