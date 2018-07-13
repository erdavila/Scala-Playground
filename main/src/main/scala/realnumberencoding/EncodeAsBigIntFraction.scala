package realnumberencoding

import numbers.Fraction

object EncodeAsBigIntFraction extends Encode {

  protected type Number = Fraction[BigInt]

  protected val fractional: Fractional[Number] = implicitly

  override protected def codeToString(code: Number): String = code + " ≅ " + code.toDouble
}
