package realnumberencoding

import realnumberencoding.StringDisplay.display
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Decode {

  def main(codes: Array[String]): Unit =
    for (codeString <- codes) {
      Try { BigDecimal(codeString) }.filter(_ >= 0) match {
        case Success(code) => decodeAndShow(code)
        case Failure(_) => println("Invalid number: " + codeString)
      }
    }

  private def decodeAndShow(code: BigDecimal): Unit = {
    println("Code: " + code)
    println()
    for (encoding <- StringRealNumberEncoding.getEncodings[BigDecimal]()(VeryBigDecimal.fractional)) {
      decodeAndShow(code, encoding)
      println()
    }
  }

  private def decodeAndShow(code: BigDecimal, encoding: StringRealNumberEncoding[BigDecimal]): Unit = {
    println(s"  Tokenized as ${encoding.description} (${encoding.tokenValues} tokens)")
    val string = encoding.decode(code)
    println("    Decoded: " + display(string))
  }
}
