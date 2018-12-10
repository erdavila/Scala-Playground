package realnumberencoding

object EncodeShortest extends Encode {

  protected type Number = BigDecimal

  protected val fractional: Fractional[Number] = VeryBigDecimal.fractional

  override protected def encode(encoding: StringRealNumberEncoding[Number])(string: String): Number =
    encoding.encodeShortest(string, base = 10)
}
