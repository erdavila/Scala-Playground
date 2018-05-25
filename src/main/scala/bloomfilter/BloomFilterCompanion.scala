package bloomfilter

import scala.language.higherKinds

trait BloomFilterCompanion extends BloomFilterParameters {

  protected type BF[A] <: BloomFilter[A]

  def apply[A](chanceOfFalsePositive: Double, hashFunction: A => Int)(elems: A*): BF[A]#Self =
    apply(chanceOfFalsePositive, elems.size, hashFunction) ++ elems

  def apply[A](chanceOfFalsePositive: Double, expectedSize: Int, hashFunction: A => Int): BF[A] = {
    require(chanceOfFalsePositive > 0.0)
    require(chanceOfFalsePositive < 1.0)
    require(expectedSize >= 1)
    val numOfHashes = optimalNumOfHashes(chanceOfFalsePositive)
    val numOfBits = numberOfBits(expectedSize, chanceOfFalsePositive)
    apply(numOfHashes, numOfBits, hashFunction)
  }

  def apply[A](numOfHashes: Int, numOfBits: Int, hashFunction: A => Int): BF[A]
}
