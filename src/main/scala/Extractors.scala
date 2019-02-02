object Extractors extends App {
  {
    println("Extraction defined with unapply()...")

    class Class {
      def unapply(n: Int): Option[(Int, Int)] = Some((n / 7, n % 7))
    }

    val instance = new Class
    object /% extends Class

    10 match {
      case instance(d, r) =>
        assert(d == 1)
        assert(r == 3)
      case _ => assert(false)
    }

    10 match {
      case d instance r =>
        assert(d == 1)
        assert(r == 3)
      case _ => assert(false)
    }

    10 match {
      case d /% r =>
        assert(d == 1)
        assert(r == 3)
      case _ => assert(false)
    }

    10 match {
      case /%(d, r) =>
        assert(d == 1)
        assert(r == 3)
      case _ => assert(false)
    }

    {
      val instance(d, r) = 10
      assert(d == 1)
      assert(r == 3)
    }

    {
      val d instance r = 10
      assert(d == 1)
      assert(r == 3)
    }

    {
      val d /% r = 10
      assert(d == 1)
      assert(r == 3)
    }

    {
      val /%(d, r) = 10
      assert(d == 1)
      assert(r == 3)
    }
  }

  {
    println("Extraction defined with unapplySeq()...")

    object Parts {
      def unapplySeq(str: String): Option[Seq[String]] = Some(str.split("-"))
    }

    "abc-123-xyz" match {
      case Parts(s1, s2, s3) =>
        assert(s1 == "abc")
        assert(s2 == "123")
        assert(s3 == "xyz")
    }

    "abc-123-xyz" match {
      case Parts(first, rest @ _*) =>
        assert(first == "abc")
        assert(rest == Seq("123", "xyz"))
    }

    {
      val Parts(s1, s2, s3) = "abc-123-xyz"
      assert(s1 == "abc")
      assert(s2 == "123")
      assert(s3 == "xyz")
    }

    {
      val Parts(first, rest @_*) = "abc-123-xyz"
      assert(first == "abc")
      assert(rest == Seq("123", "xyz"))
    }
  }

  {
    println("Extraction defined with case class...")

    case class +*+(c: Char, i: Int)
    val a = +*+('@', 7)

    a match {
      case x +*+ y =>
        assert(x == '@')
        assert(y == 7)
      case _ =>
        assert(false)
    }

    a match {
      case +*+(x, y) =>
        assert(x == '@')
        assert(y == 7)
      case _ =>
        assert(false)
    }

    {
      val x +*+ y = a
      assert(x == '@')
      assert(y == 7)
    }

    {
      val +*+(x, y) = a
      assert(x == '@')
      assert(y == 7)
    }
  }

  {
    println("Boolean pattern matching...")

    case class multipleOf(of: Int) {
      def unapply(i: Int): Boolean = i % of == 0
    }

    val multipleOf3 = multipleOf(3)
    val multipleOf5 = multipleOf(5)
    val multipleOf15 = multipleOf(15)

    def fizzBuzz(n: Int) = n match {
      case multipleOf15() => "FizzBuzz"
      case multipleOf5() => "Buzz"
      case multipleOf3() => "Fizz"
      case _ => ""
    }

    assert(fizzBuzz(10) == "Buzz")
    assert(fizzBuzz(11) == "")
    assert(fizzBuzz(12) == "Fizz")
    assert(fizzBuzz(13) == "")
    assert(fizzBuzz(14) == "")
    assert(fizzBuzz(15) == "FizzBuzz")
  }

  {
    println("@ operator...")

    val x = Seq(Option(1), Option(2), Option(3))

    {
      x match {
        case a @ Seq(b @ Some(c), d @ _*) =>
          assert(a == Seq(Option(1), Option(2), Option(3)))
          assert(b == Some(1))
          assert(c == 1)
          assert(d == Seq(Option(2), Option(3)))
      }
    }

    {
      val a @ Seq(b @ Some(c), d @ _*) = x
      assert(a == Seq(Option(1), Option(2), Option(3)))
      assert(b == Some(1))
      assert(c == 1)
      assert(d == Seq(Option(2), Option(3)))
    }
  }

  println("OK")
}
