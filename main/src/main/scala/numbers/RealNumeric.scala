package numbers

import scala.math.Numeric.BigDecimalIsFractional
import scala.math.Numeric.DoubleIsFractional
import scala.math.Numeric.FloatIsFractional

trait RealNumeric[A] extends Fractional[A] {

}

object RealNumeric {

  trait FloatIsRealNumeric extends RealNumeric[Float] with FloatIsFractional with Ordering.FloatOrdering
  implicit object FloatIsRealNumeric extends FloatIsRealNumeric

  trait DoubleIsRealNumeric extends RealNumeric[Double] with DoubleIsFractional with Ordering.DoubleOrdering
  implicit object DoubleIsRealNumeric extends DoubleIsRealNumeric

  trait BigDecimalIsRealNumeric extends RealNumeric[BigDecimal] with BigDecimalIsFractional with Ordering.BigDecimalOrdering
  implicit object BigDecimalIsRealNumeric extends BigDecimalIsRealNumeric
}
