package monads

object TestSuite {
  def main(args: Array[String]): Unit = {
    MaybeMonadTest.main(args)
    ListMonadTest.main(args)
    IdentityMonadTest.main(args)
  }
}
