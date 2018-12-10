package transposition.test

object TestSuite {

  def main(args: Array[String]): Unit = {
    StandardSequencesTest.main(args)
    CustomSequencesTest.main(args)
    MixedSequencesTest.main(args)
    println("OK!")
  }
}
