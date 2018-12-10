package realnumberencoding

import scala.annotation.tailrec

object RealNumberEncoding {
  type Token = Int
}

class RealNumberEncoding[N](tokenValues: Int)(implicit frac: Fractional[N]) {
  import RealNumberEncoding.Token
  import frac._

  private val tokenValuesFrac = frac.fromInt(tokenValues)

  def encode(tokens: Stream[Token]): N = {
    val (code, size) = tokens.foldLeft((frac.zero, 0)) { case ((code, size), token) =>
      require(token < tokenValues)
      val newCode = (code + frac.fromInt(token)) / tokenValuesFrac
      (newCode, size + 1)
    }

    code + frac.fromInt(size)
  }

  def decode(code: N): Stream[Token] = {
    def intAndFracParts(n: N): (Int, N) = {
      val int = n.toInt()
      val frac = n - this.frac.fromInt(int)
      (int, frac)
    }

    val (size, encoded) = intAndFracParts(code)

    Stream.iterate((encoded, 0)) { case (code, _) =>
      val scaledCode = code * tokenValuesFrac
      val (token, newCode) = intAndFracParts(scaledCode)
      (newCode, token)
    }.map { case (_, token) => token }.drop(1).take(size)
  }
}
