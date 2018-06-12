package realnumberencoding

object EncodeAsBigDecimal extends Encode {

  protected type Number = BigDecimal

  protected val fractional: Fractional[Number] = VeryBigDecimal.fractional
}
