package bloomfilter

import scala.math.exp
import scala.math.log
import scala.math.pow

trait BloomFilterParameters {

  private val LogOf2 = log(2)

  def chanceOfFalsePositive(size: Int, numOfBits: Int): Double = {
    val numOfHashes = optimalNumOfHashes(size, numOfBits)
    chanceOfFalsePositive(size, numOfBits, numOfHashes)
  }

  def chanceOfFalsePositive(size: Int, numOfBits: Int, numOfHashes: Int): Double =
    pow(1 - exp(-numOfHashes * size / numOfBits.toDouble), numOfHashes)

  def optimalNumOfHashes(size: Int, numOfBits: Int): Int =
    (LogOf2 * numOfBits / size).round.toInt max 1

  def optimalNumOfHashes(chanceOfFalsePositive: Double): Int =
    (-log(chanceOfFalsePositive) / LogOf2).round.toInt max 1

  def numberOfBits(size: Int, chanceOfFalsePositive: Double): Int =
    // Assuming optimal number of hashes
    (-size * log(chanceOfFalsePositive) / (LogOf2 * LogOf2)).round.toInt

  def numberOfBits(size: Int, chanceOfFalsePositive: Double, numOfHashes: Int): Int =
    (-numOfHashes * size / log(1 - pow(chanceOfFalsePositive, 1.0 / numOfHashes))).round.toInt

  def estimatedSize(numOfBits: Int, numOfHashes: Int, bitsSetToOne: Int): Double =
    -log(1 - bitsSetToOne / numOfBits.toDouble) * numOfBits / numOfHashes
}
