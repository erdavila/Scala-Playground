package realnumberencoding

import realnumberencoding.StringDisplay.display
import realnumberencoding.tokenizing.StringTokenizing

trait Encode {

  protected type Number

  protected val fractional: Fractional[Number]

  protected def codeToString(code: Number): String =
    code.toString()

  protected def encode(encoding: StringRealNumberEncoding[Number])(string: String): Number =
    encoding.encode(string.reverse)

  def main(strings: Array[String]): Unit =
    for (string <- strings) {
      encodeAndShow(string)
    }

  private def encodeAndShow(string: String): Unit = {
    println("String: " + display(string))
    println()
    for (encoding <- StringRealNumberEncoding.getEncodings[Number]()(fractional)) {
      encodeAndShow(string, encoding)
      println()
    }
  }

  private def encodeAndShow(string: String, encoding: StringRealNumberEncoding[Number]): Unit = {
    println(s"  Tokenized as ${encoding.description} (${encoding.tokenValues} tokens)")

    try {
      val code = encode(encoding)(string)
      println("    Encoded: " + codeToString(code))

      val decoded = encoding.decode(code)
      println("    Decoded: " + display(decoded))
    } catch {
      case _: StringTokenizing.Exception =>
        println("    It is not possible to use this tokenization")
    }
  }
}
