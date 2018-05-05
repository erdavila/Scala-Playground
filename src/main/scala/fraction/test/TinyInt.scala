package fraction.test

import scala.language.implicitConversions

class TinyInt private(val value: Byte) extends AnyVal {
  import TinyInt.FromInt

  def +(that: TinyInt): TinyInt = (this.value + that.value).toTinyInt
  def -(that: TinyInt): TinyInt = (this.value - that.value).toTinyInt
  def *(that: TinyInt): TinyInt = (this.value * that.value).toTinyInt
  def /(that: TinyInt): TinyInt = (this.value / that.value).toTinyInt
  def %(that: TinyInt): TinyInt = (this.value % that.value).toTinyInt
  def unary_- : TinyInt = (-value).toTinyInt

  /*
  def &(that: TinyInt): TinyInt = ???
  def |(that: TinyInt): TinyInt = ???
   */
  def unary_~ : Int = ~value

  /*
  def ==(that: TinyInt): TinyInt = ???
  def !=(that: TinyInt): TinyInt = ???
  def <(that: TinyInt): TinyInt = ???
  def >(that: TinyInt): TinyInt = ???
  def <=(that: TinyInt): TinyInt = ???
  def >=(that: TinyInt): TinyInt = ???
   */
  def compareTo(that: TinyInt): Int = this.value compareTo that.value

  def ==[I](that: I)(implicit integral: Integral[I]): Boolean = { import integral._; this.toInt == that.toInt }
  def !=[I](that: I)(implicit integral: Integral[I]): Boolean = { import integral._; this.toInt != that.toInt }

  def toInt: Int = value
  def toLong: Long = value
  def toFloat: Float = value
  def toDouble: Double = value

  override def toString(): String = value.toString()
}

object TinyInt {

  val NumBits: Int = 4

  private val NegativeCheckMask = 1 << (NumBits - 1)
  private val NegativeMask = -1 << NumBits
  private val NonNegativeMask = ~NegativeMask

  val Zero: TinyInt = 0.toTinyInt
  val One: TinyInt = 1.toTinyInt

  // ...11 00..(numBits - 1)..00
  val MinValue: TinyInt = (-1 << (NumBits - 1)).toTinyInt

  // ...00 11..(numBits - 1)..11
  val MaxValue: TinyInt = (~MinValue).toTinyInt

  implicit class FromInt(val value: Int) extends AnyVal {
    def toTinyInt: TinyInt = {
      val negative = (value & NegativeCheckMask) != 0
      val maskedValue =
        if (negative) {
          value | NegativeMask
        } else {
          value & NonNegativeMask
        }
      new TinyInt(maskedValue.toByte)
    }
  }

  implicit def toInt(tiny: TinyInt): Int = tiny.toInt

  implicit val integral: Integral[TinyInt] =
    new Integral[TinyInt] {
      def plus(x: TinyInt, y: TinyInt): TinyInt = x + y
      def minus(x: TinyInt, y: TinyInt): TinyInt = x - y
      def times(x: TinyInt, y: TinyInt): TinyInt = x * y
      def quot(x: TinyInt, y: TinyInt): TinyInt = x / y
      def rem(x: TinyInt, y: TinyInt): TinyInt = x % y

      def negate(x: TinyInt): TinyInt = -x
      def compare(x: TinyInt, y: TinyInt): Int = x compareTo y

      def fromInt(x: Int): TinyInt = x.toTinyInt

      def toInt(x: TinyInt): Int = x.toInt
      def toLong(x: TinyInt): Long = x.toLong
      def toFloat(x: TinyInt): Float = x.toFloat
      def toDouble(x: TinyInt): Double = x.toDouble
    }
}
