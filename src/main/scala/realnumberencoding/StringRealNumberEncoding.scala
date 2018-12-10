package realnumberencoding

import realnumberencoding.RealNumberEncoding.Token
import realnumberencoding.tokenizing.AlphanumericPlusSpaceTokenizing
import realnumberencoding.tokenizing.AsciiStringTokenizing
import realnumberencoding.tokenizing.StringTokenizing
import realnumberencoding.tokenizing.UppercaseLettersPlusSpaceTokenizing
import realnumberencoding.tokenizing.Utf8StringTokenizing
import scala.annotation.tailrec

object StringRealNumberEncoding {

  def getEncodings[N: Fractional]() =
    Seq(
      new StringRealNumberEncoding(Utf8StringTokenizing),
      new StringRealNumberEncoding(AsciiStringTokenizing),
      new StringRealNumberEncoding(AlphanumericPlusSpaceTokenizing),
      new StringRealNumberEncoding(UppercaseLettersPlusSpaceTokenizing),
    )
}

class StringRealNumberEncoding[N](tokenizing: StringTokenizing)(implicit fractional: Fractional[N]) {
  import fractional._

  private val Debug = false

  private val realNumberEncoding = new RealNumberEncoding[N](tokenizing.tokenValues)

  val tokenValues = tokenizing.tokenValues
  val description = tokenizing.description

  def encode(string: String): N = {
    val tokens = tokenizing.tokenize(string)
    realNumberEncoding.encode(tokens)
  }

  def encodeShortest(string: String, base: Int): N = {
    val targetTokens = tokenizing.tokenize(string)

    @tailrec
    def loop(code: N, fraction: N): N = {
      val Some((newCode, cmp)) = (0 until base).reverse.toStream map { n =>
        val delta = fromInt(n) * fraction
        val newCode = code + delta
        val tokens = realNumberEncoding.decode(newCode)
        if (Debug) println(s"[$n] $code + $delta = $newCode: ${StringDisplay.display(tokenizing.detokenize(tokens))}")
        val cmp = compare(tokens, targetTokens)
        (newCode, cmp)
      } find { case (_, cmp) => cmp <= 0 }
      if (Debug) println("Best new code: " + newCode)

      if (cmp == 0) {
        newCode
      } else {
        loop(newCode, fraction / fromInt(base))
      }
    }

    loop(fromInt(targetTokens.length), one / fromInt(base))
  }

  private def compare(tokensA: Seq[Token], tokensB: Seq[Token]): Int =
    (tokensA zip tokensB).toStream map { case (tokenA, tokenB) => tokenA - tokenB } find { _ != 0 } getOrElse 0

  def decode(code: N): String = {
    val tokens = realNumberEncoding.decode(code)
    tokenizing.detokenize(tokens)
  }
}
