package bloomfilter.test

import bloomfilter.BloomFilter
import bloomfilter.DefinitivelyNot
import bloomfilter.Possibly
import bloomfilter.immutable
import bloomfilter.mutable
import scala.annotation.tailrec
import scala.util.Random
import shapeless._
import shapeless.tag.@@

object Test {

  type Data = Int

  sealed trait Outcome
  case object TrueNegative extends Outcome
  case object TruePositive extends Outcome
  case object FalsePositive extends Outcome

  val SampleSize = 100000

  def main(args: Array[String]): Unit = {
    printMeaning[FPChance]("chance of false positive")
    printMeaning[FPRate]("measured rate of false positive")
    printMeaning[Size]("number of elements")
    printMeaning[EstimatedSize]("estimated number of elements")
    printMeaning[NumOfHashes]("number of hashes")
    printMeaning[NumOfBits]("number of bits")
    printMeaning[BitsPerEntry]("bits per element")
    println()

    varyingChanceOfFalsePositive()
    varyingSize()
    varyingNumOfHashes()
    varyingNumOfBits()
  }

  object printMeaning {
    def apply[P] = new PrintMeaning[P]

    class PrintMeaning[P] {
      def apply[A](meaning: String)(implicit p: Parameter[A @@ P]): Unit =
        println(p.name + ": " + meaning)
    }
  }

  def varyingChanceOfFalsePositive(): Unit = {
    val size = 10000

    val headers = Size(size) :: "optimal k" :: HNil
    val values = Seq(0.001, 0.005, 0.01, 0.05, 0.1, 0.5)
    varying[FPChance](headers, values) { fpChance =>
      val (bloomFilter, fpRate) = simulate(size, fpChance)
      val bitsPerEntry = bloomFilter.numOfBits / size.toDouble
      val row =
        NumOfHashes(bloomFilter) ::
        NumOfBits(bloomFilter) ::
        BitsPerEntry(bitsPerEntry) ::
        FPRate(fpRate) ::
        EstimatedSize(bloomFilter) ::
        HNil
      row
    }
  }

  def varyingSize(): Unit = {
    val chanceOfFalsePositive = 0.01

    val headers = FPChance(chanceOfFalsePositive) :: "optimal k" :: HNil
    val values = Seq(1000, 5000, 10000, 50000, 100000)
    varying[Size](headers, values) { size =>
      val (bloomFilter, fpRate) = simulate(size, chanceOfFalsePositive)
      val bitsPerEntry = bloomFilter.numOfBits / size.toDouble
      val row =
        NumOfHashes(bloomFilter) ::
        NumOfBits(bloomFilter) ::
        BitsPerEntry(bitsPerEntry) ::
        FPRate(fpRate) ::
        EstimatedSize(bloomFilter) ::
        HNil
      row
    }
  }

  def varyingNumOfHashes(): Unit = {
    val size = 10000
    val chanceOfFalsePositive = 0.01
    val optimalNumOfHashes = BloomFilter.optimalNumOfHashes(chanceOfFalsePositive)
    val numOfBits = BloomFilter.numberOfBits(size, chanceOfFalsePositive, optimalNumOfHashes)
    val bitsPerEntry = numOfBits / size.toDouble

    val w = Witness(optimalNumOfHashes)
    val headers = Size(size) :: NumOfBits(numOfBits) :: BitsPerEntry(bitsPerEntry) :: HNil
    val values = (optimalNumOfHashes * 0.5).floor.toInt to (optimalNumOfHashes * 1.5).ceil.toInt
    varying[NumOfHashes MarkedWhen w.T](headers, values) { numOfHashes =>
      val fpChance = BloomFilter.chanceOfFalsePositive(size, numOfBits, numOfHashes)
      val (bloomFilter, fpRate) = simulate(size, numOfHashes, numOfBits)
      val row =
        FPChance(fpChance) ::
        FPRate(fpRate) ::
        EstimatedSize(bloomFilter) ::
        HNil
      row
    }
  }

  def varyingNumOfBits(): Unit = {
    val size = 10000
    val chanceOfFalsePositive = 0.01
    val numOfHashes = BloomFilter.optimalNumOfHashes(chanceOfFalsePositive)
    val recommendedNumOfBits = BloomFilter.numberOfBits(size, chanceOfFalsePositive)

    val w = Witness(recommendedNumOfBits)
    val headers = Size(size) :: NumOfHashes(numOfHashes) :: HNil
    val values = (-3 to +3) map { recommendedNumOfBits + 1000 * _ }
    varying[NumOfBits MarkedWhen w.T](headers, values) { numOfBits =>
      val (bloomFilter, fpRate) = simulate(size, numOfHashes, numOfBits)
      val fpChance = BloomFilter.chanceOfFalsePositive(size, numOfBits, numOfHashes)
      val bitsPerEntry = bloomFilter.numOfBits / size.toDouble
      val row =
        FPChance(fpChance) ::
        BitsPerEntry(bitsPerEntry) ::
        FPRate(fpRate) ::
        EstimatedSize(bloomFilter) ::
        HNil
      row
    }
  }

  object varying {
    def apply[V] = new Varying[V]

    class Varying[V] {
      def apply[HH <: HList, A, PP <: HList]
        (headers: HH, values: Seq[A])
        (f: (A @@ V) => PP)
        (implicit
          hh: Headers[HH],
          p: Parameter[A @@ V],
          pp: Parameters[(A @@ V) :: PP],
        ): Unit =
      {
        println("Varying " + p.name + "; with " + hh.format(headers).mkString("; ") + ":")

        val formattedRows =
          for (value <- values)
          yield {
            val taggedValue = tag[V](value)
            val row = taggedValue :: f(taggedValue)
            pp.format(row)
          }

        val lengths =
          formattedRows.foldLeft(pp.zeroLengths()) { (lengths, formattedRow) =>
            pp.maxLengths(lengths, formattedRow)
          }

        for (formattedRow <- formattedRows) {
          val strs = pp.withNameAndPadded(lengths)(formattedRow)
          println("  " + strs.head + " => " + strs.tail.mkString("; "))
        }

        println()
      }
    }
  }

  def simulate(size: Int, chanceOfFalsePositive: Double): (BloomFilter[Data], Double) =
    simulate(
      size,
      { hashFunction => mutable.BloomFilter[Data](chanceOfFalsePositive, size, hashFunction) },
      { hashFunction => immutable.BloomFilter[Data](chanceOfFalsePositive, size, hashFunction) }
    )

  def simulate(size: Int, numOfHashes: Int, numOfBits: Int): (BloomFilter[Data], Double) =
    simulate(
      size,
      { hashFunction => mutable.BloomFilter[Data](numOfHashes, numOfBits, hashFunction) },
      { hashFunction => immutable.BloomFilter[Data](numOfHashes, numOfBits, hashFunction) }
    )

  def simulate(
    size: Int,
    mut: (Data => Int) => mutable.BloomFilter[Data],
    immut: (Data => Int) => immutable.BloomFilter[Data]
  ): (BloomFilter[Data], Double) = {
    val set: Set[Data] = generateDataSet(size)

    val hashFunction: Data => Int = identity
    val bloomFilter =
      if (Random.nextBoolean()) {
        val bf = mut(hashFunction)
        for (data <- set.grouped(10)) {
          bf ++= data
        }
        bf
      } else {
        val bf0 = immut(hashFunction)
        set
          .grouped(10)
          .foldLeft(bf0) { (bloomFilter, dataGroup) => bloomFilter ++ dataGroup }
      }

    val counters =
      Seq.fill(SampleSize) { generateData() }
        .map { data =>
          bloomFilter.contains(data) match {
            case Possibly if set.contains(data) => TruePositive
            case Possibly => FalsePositive
            case DefinitivelyNot => TrueNegative
          }
        }
        .groupBy(identity)
        .mapValues(_.size)
        .withDefaultValue(0)

    val trueNegatives = counters(TrueNegative)
    val falsePositives = counters(FalsePositive)
    val negatives = trueNegatives + falsePositives
    val falsePositivesRate = falsePositives / negatives.toDouble
    (bloomFilter, falsePositivesRate)
  }

  private def generateDataSet(size: Int): Set[Data] = {
    @tailrec
    def loop(set: Set[Data]): Set[Data] =
      if (set.size < size) {
        val data = generateData()
        loop(set + data)
      } else {
        set
      }

    loop(Set.empty)
  }

  private def generateData(): Data =
    Random.nextInt()
}
