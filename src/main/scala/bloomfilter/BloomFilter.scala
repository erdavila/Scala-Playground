package bloomfilter

import scala.annotation.tailrec
import scala.collection.BitSet
import scala.collection.GenTraversableOnce
import scala.util.Random

object BloomFilter extends BloomFilterParameters

abstract class BloomFilter[A](val numOfHashes: Int, val numOfBits: Int, val hashFunction: A => Int, bits: BitSet) {
  require(numOfHashes >= 1, numOfHashes)
  require(numOfBits >= numOfHashes)

  type Self <: BloomFilter[A]

  def ++(elems: GenTraversableOnce[A]): Self

  def +(elem1: A, elems: A*): Self =
    this ++ (elem1 +: elems)

  def contains(elem: A): ProbabilisticAnswer =
    if (hashesOf(elem) forall { bits.contains }) {
      Possibly
    } else {
      DefinitivelyNot
    }

  protected def hashesOf(value: A): Set[Int] = {
    val hash = hashFunction(value)
    val random = new Random(hash)

    @tailrec
    def loop(hashes: Set[Int]): Set[Int] =
      if (hashes.size < numOfHashes) {
        val newHash = random.nextInt(numOfBits)
        loop(hashes + newHash)
      } else {
        hashes
      }

    loop(Set.empty)
  }

  def estimatedSize(): Double =
    BloomFilter.estimatedSize(numOfBits, numOfHashes, bits.size)
}
