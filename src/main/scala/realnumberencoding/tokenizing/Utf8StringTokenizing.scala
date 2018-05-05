package realnumberencoding.tokenizing

object Utf8StringTokenizing extends CharEncodingStringTokenizing {
  override protected val Encoding = "UTF-8"
  override val description = "UTF-8 bytes"
  override val tokenValues = 256
}
