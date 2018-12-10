package realnumberencoding

import java.math.MathContext
import scala.math.Numeric.BigDecimalIsFractional

object VeryBigDecimal {

  val Precision = 160

  val fractional: Fractional[BigDecimal] =
    new BigDecimalIsFractional with Ordering.BigDecimalOrdering {
      private val mc = new MathContext(Precision)
      override def zero: BigDecimal = super.zero(mc)
      override def one: BigDecimal = super.one(mc)
      override def fromInt(x: Int): BigDecimal = super.fromInt(x)(mc)
    }
}
