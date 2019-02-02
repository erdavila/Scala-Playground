package transposition.test

import transposition._

object DSL {

  def transposed[I](input: I): Transposed[I] = new Transposed[I](input)

  class Transposed[I](input: I) {
    def must[O](implicit transposer: Transposer[I, O]): Must[I, O] = {
      val output: O = input.transposed
      new Must[I, O](input, output)
    }
  }

  class Must[I, O](input: I, output: O) {
    def be[E](expected: E)(implicit ev: E =:= O): Unit =
      assert(output == expected)
  }
}
