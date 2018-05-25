package bloomfilter.mutable

import bloomfilter.BloomFilterCompanion
import scala.collection.GenTraversableOnce
import scala.collection.mutable.BitSet

object BloomFilter extends BloomFilterCompanion {

  protected type BF[A] = BloomFilter[A]

  def apply[A](numOfHashes: Int, numOfBits: Int, hashFunction: A => Int): BloomFilter[A] =
    new BloomFilter[A](numOfHashes, numOfBits, hashFunction, BitSet.empty)
}

class BloomFilter[A] private(numOfHashes: Int, numOfBits: Int, hashFunction: A => Int, bits: BitSet)
  extends bloomfilter.BloomFilter[A](numOfHashes, numOfBits, hashFunction, bits) {

  type Self = BloomFilter[A]

  def ++(elems: GenTraversableOnce[A]): Self = {
    val newBloomFilter = new BloomFilter[A](numOfHashes, numOfBits, hashFunction, bits.clone())
    newBloomFilter ++= elems
  }

  def add(elem: A): this.type =
    this += elem

  def +=(elem1: A, elems: A*): this.type =
    this ++= (elem1 +: elems)

  def ++=(elems: GenTraversableOnce[A]): this.type = {
    for (elem <- elems) {
      bits ++= hashesOf(elem)
    }
    this
  }
}
