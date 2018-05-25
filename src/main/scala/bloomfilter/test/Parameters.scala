package bloomfilter.test

import shapeless._
import shapeless.::

trait Parameters[PP <: HList] {
  def format(params: PP): List[String]
  def zeroLengths(): List[Int]
  def maxLengths(lengths: List[Int], formattedRow: List[String]): List[Int]
  def withNameAndPadded(lengths: List[Int])(formattedRow: List[String]): List[String]
}

object Parameters {
  implicit val hnil: Parameters[HNil] =
    new Parameters[HNil] {
      def format(params: HNil): List[String] = Nil
      def zeroLengths(): List[Int] = Nil
      def maxLengths(lengths: List[Int], formattedRow: List[String]): List[Int] = Nil
      def withNameAndPadded(lengths: List[Int])(formattedRow: List[String]): List[String] = Nil
    }

  implicit def cons[H, T <: HList](implicit p: Parameter[H], pp: Parameters[T]): Parameters[H :: T] =
    new Parameters[H :: T] {
      def format(params: H :: T): List[String] = {
        val h = p.format(params.head)
        val t = pp.format(params.tail)
        h :: t
      }

      def zeroLengths(): List[Int] = 0 :: pp.zeroLengths()

      def maxLengths(lengths: List[Int], formattedRow: List[String]): List[Int] = {
        val h = lengths.head max formattedRow.head.length()
        val t = pp.maxLengths(lengths.tail, formattedRow.tail)
        h :: t
      }

      def withNameAndPadded(lengths: List[Int])(formattedRow: List[String]): List[String] = {
        val h = withNameAndPadded(lengths.head)(formattedRow.head)
        val t = pp.withNameAndPadded(lengths.tail)(formattedRow.tail)
        h :: t
      }

      private def withNameAndPadded(length: Int)(formatted: String): String = {
        val padding = " " * (length - formatted.length())
        p.name + " = " + padding + formatted
      }
    }
}
