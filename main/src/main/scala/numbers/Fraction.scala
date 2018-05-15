package numbers

import scala.annotation.tailrec

class Fraction[I] private(val num: I, val den: I)(implicit integral: Integral[I]) {
  // Algorithms adapted from https://www.boost.org/doc/libs/1_67_0/boost/rational.hpp

  import integral._

  def +(that: Fraction[I]): Fraction[I] = additiveOperation(that)(_ + _)
  def -(that: Fraction[I]): Fraction[I] = additiveOperation(that)(_ - _)

  @inline
  private def additiveOperation(that: Fraction[I])(op: (I, I) => I): Fraction[I] = {
    val gcd1 = Fraction.gcd(this.den, that.den)
    val den1 = this.den / gcd1
    val num1 = op(this.num * (that.den / gcd1), that.num * den1)

    val gcd2 = Fraction.gcd(num1, gcd1)
    val den2 = den1 * that.den/gcd2
    val num2 = num1 / gcd2

    Fraction[I](num2, den2)
  }

  def *(that: Fraction[I]): Fraction[I] = multiplicativeOperation(that.num, that.den)
  def /(that: Fraction[I]): Fraction[I] = multiplicativeOperation(that.den, that.num)

  @inline
  private def multiplicativeOperation(a: I, b: I): Fraction[I] = {
    val gcd1 = Fraction.gcd(this.num, b)
    val gcd2 = Fraction.gcd(a, this.den)
    val n = (this.num / gcd1) * (a / gcd2)
    val d = (this.den / gcd2) * (b / gcd1)
    Fraction[I](n, d)
  }

  def unary_- : Fraction[I] = Fraction[I](-num, den)
  def abs: Fraction[I] = if (num < zero) -this else this

  def ==(that: Fraction[I]): Boolean = this.num == that.num  &&  this.den == that.den
  def !=(that: Fraction[I]): Boolean = !(this == that)

  def <(that: Fraction[I]): Boolean = {
    case class S(n: I, d: I, q: I, r: I) {
      def next(): S = S(n = d, d = r, q = d / r, r = d % r)
    }

    object S {
      @tailrec
      private def normalize(q: I, r: I, d: I): (I, I) =
        if (r < zero) {
          normalize(q - one, r + d, d)
        } else {
          (q, r)
        }

      def from(f: Fraction[I]): S = {
        val (q, r) = normalize(f.num / f.den, f.num % f.den, f.den)
        S(n = f.num, d = f.den, q, r)
      }
    }

    @tailrec
    def loop(ts: S, rs: S, reverse: Boolean): Boolean =
      if (ts.q != rs.q) {
        if (reverse) (ts.q > rs.q) else (ts.q < rs.q)
      } else {
        if (ts.r == zero  ||  rs.r == zero) {
          if (ts.r == rs.r) {
            false
          } else {
            (ts.r != zero) == reverse
          }
        } else {
          loop(
            ts.next(),
            rs.next(),
            !reverse
          )
        }
      }

    val ts = S.from(this)
    val rs = S.from(that)

    loop(ts, rs, reverse = false)
  }

  def >(that: Fraction[I]): Boolean = that < this
  def <=(that: Fraction[I]): Boolean = !(this > that)
  def >=(that: Fraction[I]): Boolean = !(this < that)

  def compareTo(that: Fraction[I]): Int =
    if (this == that) {
      0
    } else if (this < that) {
      -1
    } else {
      1
    }

  def toInt: Int = this.toLong.toInt
  def toLong: Long = (num / den).toLong()
  def toFloat: Float = this.toDouble.toFloat
  def toDouble: Double = num.toDouble() / den.toDouble()

  override def toString(): String = num.toString() + "/" + den.toString()
}

object Fraction {

  def apply[I](value: I)(implicit integral: Integral[I]): Fraction[I] =
    apply(value, integral.one)

  def apply[I](num: I, den: I)(implicit integral: Integral[I]): Fraction[I] = {
    import integral._
    require(den != zero)

    val gcd = this.gcd(num, den)
    val n = num / gcd
    val d = den / gcd

    if (d < zero) {
      new Fraction[I](-n, -d)
    } else {
      new Fraction[I](n, d)
    }
  }

  implicit def fractional[I](implicit integral: Integral[I]): Fractional[Fraction[I]] =
    new Fractional[Fraction[I]] {
      def plus(x: Fraction[I], y: Fraction[I]): Fraction[I] = x + y
      def minus(x: Fraction[I], y: Fraction[I]): Fraction[I] = x - y
      def times(x: Fraction[I], y: Fraction[I]): Fraction[I] = x * y
      def div(x: Fraction[I], y: Fraction[I]): Fraction[I] = x / y

      def negate(x: Fraction[I]): Fraction[I] = -x
      def compare(x: Fraction[I], y: Fraction[I]): Int = x compareTo y

      def fromInt(x: Int): Fraction[I] = apply(integral.fromInt(x))

      def toInt(x: Fraction[I]): Int = x.toInt
      def toLong(x: Fraction[I]): Long = x.toLong
      def toFloat(x: Fraction[I]): Float = x.toFloat
      def toDouble(x: Fraction[I]): Double = x.toDouble
    }

  @tailrec
  private def gcd[I](a: I, b: I)(implicit integral: Integral[I]): I = {
    import integral._
    if (b == zero) {
      a
    } else {
      gcd(b, a % b)
    }
  }
}
