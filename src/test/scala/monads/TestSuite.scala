package monads

object TestSuite {
  def main(args: Array[String]): Unit = {
    MaybeMonadTest.main(args)
    ListMonadTest.main(args)
    IdentityMonadTest.main(args)
    IOMonadTest.main(args)
    WriterMonadTest.main(args)
    ReaderMonadTest.main(args)
    StateMonadTest.main(args)
    ContinuationMonadTest.main(args)
  }
}
