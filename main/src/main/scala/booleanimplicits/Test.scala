package booleanimplicits

import shapeless.test.illTyped

object Test {

  private trait IsInt[A]
  private implicit object isInt extends IsInt[Int]

  private trait IsString[A]
  private implicit object isString extends IsString[String]

  def main(args: Array[String]): Unit = {
    testNot()
    testAnd()
    testOr()
    testXor()
    println("OK")
  }

  private def testNot(): Unit = {
    def notInt[A](implicit not: Not[IsInt[A]]) = not

    {
      val not = notInt[String]
      assert(not == Not)
    }
    illTyped("notInt[Int]", ".*could not find implicit.*")
  }

  private def testAnd(): Unit = {
    def intAndString[A, B](implicit and: IsInt[A] And IsString[B]) = and

    {
      val and = intAndString[Int, String]
      assert(and == And(isInt, isString))
    }
    illTyped("intAndString[Int, Char]",      ".*could not find implicit.*")
    illTyped("intAndString[Double, String]", ".*could not find implicit.*")
    illTyped("intAndString[Double, Char]",   ".*could not find implicit.*")
  }

  private def testOr(): Unit = {
    def intOrString[A, B](implicit or: IsInt[A] Or IsString[B]) = or

    {
      val or = intOrString[Int, String]
      assert(or == Or(Some(isInt), Some(isString)))
    }

    {
      val or = intOrString[Int, Char]
      assert(or == Or(Some(isInt), None))
    }

    {
      val or = intOrString[Double, String]
      assert(or == Or(None, Some(isString)))
    }

    illTyped("intOrString[Double, Char]", ".*could not find implicit.*")
  }

  private def testXor(): Unit = {
    def intXorString[A, B](implicit xor: IsInt[A] Xor IsString[B]) = xor

    {
      val xor = intXorString[Int, Char]
      assert(xor == Xor(Left(isInt)))
    }

    {
      val xor = intXorString[Double, String]
      assert(xor == Xor(Right(isString)))
    }

    illTyped("intXorString[Int, String]",  ".*could not find implicit.*")
    illTyped("intXorString[Double, Char]", ".*could not find implicit.*")
  }
}
