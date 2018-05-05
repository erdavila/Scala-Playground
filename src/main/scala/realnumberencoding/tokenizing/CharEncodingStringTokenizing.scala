package realnumberencoding.tokenizing

trait CharEncodingStringTokenizing extends StringTokenizing {
  protected val Encoding: String
  val description: String

  def tokenize(string: String): Stream[Token] =
    string.getBytes(Encoding).toStream map { _ & 0x00FF }

  def detokenize(tokens: Stream[Token]): String =
    new String(tokens.map(_.toByte).toArray, Encoding)
}
