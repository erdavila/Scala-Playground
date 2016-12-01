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

  println("OK")
}
