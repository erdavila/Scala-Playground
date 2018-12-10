package bloomfilter.immutable

import bloomfilter.BloomFilterCompanion
import scala.collection.GenTraversableOnce
import scala.collection.immutable.BitSet

object BloomFilter extends BloomFilterCompanion {

  protected type BF[A] = BloomFilter[A]

  def apply[A](numOfHashes: Int, numOfBits: Int, hashFunction: A => Int): BloomFilter[A] =
    new BloomFilter[A](numOfHashes, numOfBits, hashFunction, BitSet.empty)
}

class BloomFilter[A] private(numOfHashes: Int, numOfBits: Int, hashFunction: A => Int, bits: BitSet)
  extends bloomfilter.BloomFilter[A](numOfHashes, numOfBits, hashFunction, bits) {

  type Self = BloomFilter[A]

  def ++(elems: GenTraversableOnce[A]): Self = {
    val newBits = elems.foldLeft(bits) { (bits, elem) => bits ++ hashesOf(elem) }
    new BloomFilter[A](numOfHashes, numOfBits, hashFunction, newBits)
  }
}
