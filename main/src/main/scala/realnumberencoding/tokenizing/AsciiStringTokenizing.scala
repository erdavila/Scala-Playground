package realnumberencoding.tokenizing

object AsciiStringTokenizing extends CharEncodingStringTokenizing {
  override protected val Encoding = "ASCII"
  override val description = "ASCII bytes"
  override val tokenValues = 128
}
