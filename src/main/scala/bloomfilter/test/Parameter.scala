package bloomfilter.test

import bloomfilter.BloomFilter
import shapeless._
import shapeless.tag.@@

trait BitsPerEntry
object BitsPerEntry {
  def apply(value: Double) = tag[BitsPerEntry](value)
}

trait EstimatedSize
object EstimatedSize {
  def apply(bloomFilter: BloomFilter[_]) = tag[EstimatedSize](bloomFilter.estimatedSize())
}

trait FPChance
object FPChance {
  def apply(value: Double) = tag[FPChance](value)
}

trait FPRate
object FPRate {
  def apply(value: Double) = tag[FPRate](value)
}

trait NumOfBits
object NumOfBits {
  def apply(value: Int) = tag[NumOfBits](value)
  def apply(bloomFilter: BloomFilter[_]): Int @@ NumOfBits = apply(bloomFilter.numOfBits)
}

trait NumOfHashes
object NumOfHashes {
  def apply(value: Int) = tag[NumOfHashes](value)
  def apply(bloomFilter: BloomFilter[_]): Int @@ NumOfHashes = apply(bloomFilter.numOfHashes)
}

trait Size
object Size {
  def apply(value: Int) = tag[Size](value)
}

trait MarkedWhen[P, X]

trait Parameter[A] {
  val name: String
  def format(value: A): String
}

object Parameter {
  implicit val bitsPerEntry: Parameter[Double @@ BitsPerEntry] = doubleParameter[BitsPerEntry]("m/n", decimalDigits = 2)
  implicit val estimatedSize: Parameter[Double @@ EstimatedSize] = doubleParameter[EstimatedSize]("n*", decimalDigits = 2)
  implicit val fpChance: Parameter[Double @@ FPChance] = rateParameter[FPChance]("p")
  implicit val fpRate: Parameter[Double @@ FPRate] = rateParameter[FPRate]("p*")
  implicit val numOfBits: Parameter[Int @@ NumOfBits] = intParameter[NumOfBits]("m")
  implicit val numOfHashes: Parameter[Int @@ NumOfHashes] = intParameter[NumOfHashes]("k")
  implicit val size: Parameter[Int @@ Size] = intParameter[Size]("n")

  private def rateParameter[P](name: String): Parameter[Double @@ P] = doubleParameter[P](name, decimalDigits = 5)

  private def intParameter[P](_name: String): Parameter[Int @@ P] =
    new Parameter[Int @@ P] {
      val name: String = _name
      def format(value: Int @@ P): String = "%d".format(value)
    }

  private def doubleParameter[P](_name: String, decimalDigits: Int): Parameter[Double @@ P] =
    new Parameter[Double @@ P] {
      val name: String = _name
      def format(value: Double @@ P): String = {
        val stringFormat = "%." + decimalDigits + "f"
        stringFormat.format(value)
      }
    }

  implicit def markedWhen[P, X <: A, A <: AnyVal](implicit p: Parameter[A @@ P], w: Witness.Aux[X]): Parameter[A @@ (P MarkedWhen X)] =
    new Parameter[A @@ (P MarkedWhen X)] {
      val name: String = p.name
      def format(value: A @@ (P MarkedWhen X)): String = {
        val retaggedValue: A @@ P = tag[P](value)
        val formatted = p.format(retaggedValue)
        val mark = if (value == w.value) "(*)" else "   "
        formatted + mark
      }
    }
}
