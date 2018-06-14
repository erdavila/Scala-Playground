package transposed.test

import transposed._

object DSL {

  def transposed[I](input: I): Transposed[I] = new Transposed[I](input)

  class Transposed[I](input: I) {
    def must[O](implicit transposer: Transposer.Aux[I, O]): Must[I, O] = {
      val output: O = input.transposed
      new Must[I, O](input, output)
    }
  }

  class Must[I, O](input: I, output: O) {
    def be[E](expected: E)(implicit ev: E =:= O): Unit =
      assert(output == expected)
  }
}
