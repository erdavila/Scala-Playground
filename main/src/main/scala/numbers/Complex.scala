package numbers

class Complex[A](val realPart: A, val imaginaryPart: A)(implicit realNumeric: RealNumeric[A]) {

  def this(realPart: A)(implicit realNumeric: RealNumeric[A]) =
    this(realPart, realNumeric.zero)

  import realNumeric._

  def +(that: Complex[A]): Complex[A] = Complex(this.realPart + that.realPart, this.imaginaryPart + that.imaginaryPart)
  def -(that: Complex[A]): Complex[A] = Complex(this.realPart - that.realPart, this.imaginaryPart - that.imaginaryPart)

  def *(that: Complex[A]): Complex[A] = {
    val a = this.realPart
    val b = this.imaginaryPart

    val c = that.realPart
    val d = that.imaginaryPart

    val realPart = this.realPart * that.realPart - this.imaginaryPart * that.imaginaryPart
    val imaginaryPart = this.imaginaryPart * that.realPart + this.realPart * that.imaginaryPart

    Complex(realPart, imaginaryPart)
  }

  def /(that: Complex[A]): Complex[A] = {
    val a = this.realPart
    val b = this.imaginaryPart

    val c = that.realPart
    val d = that.imaginaryPart

    val divisor = c*c + d*d
    val realPart = (a*c + b*d) / divisor
    val imaginaryPart = (b*c - a*d) / divisor

    Complex(realPart, imaginaryPart)
  }

  def unary_- : Complex[A] = ???

  def abs: A = ???
  def argument: A = ???
  def conjugate: Complex[A] = Complex(this.realPart, -this.imaginaryPart)
  def reciprocal: Complex[A] = ???
  def sqrt: Set[Complex[A]] = ???

  def log: Complex[A] = ???

  def pow(exp: Int): Set[Complex[A]] = ???
  def pow(exp: Complex[A]): Set[Complex[A]] = ???
  def exp: Set[Complex[A]] = ???

  def ==(that: Complex[A]): Boolean = ???
  def !=(that: Complex[A]): Boolean = ???
  def <(that: Complex[A]): Boolean = ???
  def <=(that: Complex[A]): Boolean = ???
  def >(that: Complex[A]): Boolean = ???
  def >=(that: Complex[A]): Boolean = ???
  def compareTo(that: Complex[A]): Int = ???

  def toInt: Int = ???
  def toLong: Long = ???
  def toFloat: Float = ???
  def toDouble: Double = ???
}

object Complex {

  def apply[A](real: A, imag: A)(implicit realNumeric: RealNumeric[A]): Complex[A] =
    new Complex[A](real, imag)

  def apply[A](real: A)(implicit realNumeric: RealNumeric[A]): Complex[A] =
    new Complex[A](real)

  def I[A](implicit realNumeric: RealNumeric[A]): Complex[A] =
    Complex(realNumeric.zero, realNumeric.one)

  /*
  implicit def complexAsFractional[A](implicit fractional: Fractional[A]): Fractional[Complex[A]] =
    new Fractional[Complex[A]] {
      def plus(x: Complex[A], y: Complex[A]): Complex[A] = x + y
      def minus(x: Complex[A], y: Complex[A]): Complex[A] = x - y
      def times(x: Complex[A], y: Complex[A]): Complex[A] = x * y
      def div(x: Complex[A], y: Complex[A]): Complex[A] = x / y
      def negate(x: Complex[A]): Complex[A] = -x

      def compare(x: Complex[A], y: Complex[A]): Int = x compareTo y

      def toInt(x: Complex[A]): Int = x.toInt
      def toLong(x: Complex[A]): Long = x.toLong
      def toFloat(x: Complex[A]): Float = x.toFloat
      def toDouble(x: Complex[A]): Double = x.toDouble

      def fromInt(x: Int): Complex[A] = apply(fractional.fromInt(x))
    }
   */
}
