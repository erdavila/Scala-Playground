package realnumberencoding.tokenizing

import realnumberencoding.RealNumberEncoding

object StringTokenizing {
  class Exception extends scala.Exception
}

trait StringTokenizing {
  type Token = RealNumberEncoding.Token

  val tokenValues: Int
  val description: String

  def tokenize(string: String): Stream[Token]
  def detokenize(tokens: Stream[Token]): String
}
