package collatz

import scala.annotation.tailrec
import scala.collection.mutable

case class Collatz(value: Long, sequenceSize: Int)

trait CollatzImpl {
  def longestSequence(values: Seq[Long]): Collatz

  protected def zero = Collatz(0L, 0)

  protected def nextNum(n: Long) = if (n % 2 == 0) n / 2 else 3 * n + 1

  protected def combine(collatzA: Collatz, collatzB: Collatz) =
    if (collatzA.sequenceSize > collatzB.sequenceSize) collatzA
    else collatzB
}

trait RecursiveSeqSizeCollatzImpl extends CollatzImpl {
  protected def makeAndCombine(collatzA: Collatz, valueB: Long) = {
    val collatzB = make(valueB)
    combine(collatzA, collatzB)
  }

  protected def make(value: Long) = {
    val seqSz = seqSize(value)
    Collatz(value, seqSz)
  }

  protected def seqSize(n: Long): Int = seqSize(n, 1)

  @tailrec
  private def seqSize(n: Long, sz: Int): Int =
    n match {
      case 1 => sz
      case n => seqSize(nextNum(n), sz + 1)
    }
}

trait CachedSeqSizeCollatzImpl extends CollatzImpl {
  protected type SeqSizeCache = Map[Long, Int]

  protected def seqSize(n: Long, cache: SeqSizeCache): (Int, SeqSizeCache) =
    cache.get(n) map { size => (size, cache) } getOrElse {
      val (size, nextCache) = calculateSeqSize(n, cache)
      val newCache = nextCache + (n -> size)
      (size, newCache)
    }

  private def calculateSeqSize(n: Long, cache: SeqSizeCache): (Int, SeqSizeCache) =
    n match {
      case 1 => (1, cache)
      case _ =>
        val nextN = nextNum(n)
        val (nextSize, nextCache) = seqSize(nextN, cache)
        (nextSize + 1, nextCache)
    }
}

trait MutablyCachedSeqSizeCollatzImpl extends CollatzImpl {
  protected type SeqSizeCache = mutable.Map[Long, Int]

  protected def seqSize(n: Long, cache: SeqSizeCache): Int =
    cache.getOrElse(n, {
      val nextN = nextNum(n)
      val nextNSize = seqSize(nextN, cache)
      val size = nextNSize + 1
      cache(n) = size
      size
    })
}

object Collatz extends App {
  val Repeats = 3
  val N = 1000000L
  val ExpectedValue = 837799
  val ExpectedSeqSize = 525
  def assertEqual[T](expected: T, real: T) = assert(expected == real, real)

  val nums = (1L until N)
  val impls = Seq(
    "Fold" -> FoldImpl,
    "CachedFold" -> CachedFoldImpl,
    "MutablyCachedFold" -> MutablyCachedFoldImpl,
    "MapReduce" -> MapReduceImpl,
    "ParallelMapReduce" -> ParallelMapReduceImpl,
    "Aggregate" -> AggregateImpl,
    "ParallelAggregate" -> ParallelAggregateImpl,
    "Procedural" -> ProceduralImpl,
    "CachedProcedural" -> CachedProceduralImpl
  )

  for ((name, impl) <- impls) {
    print(s"${name}: ")

    for (_ <- 1 to 3) { impl.longestSequence(nums) }
    val durationsAndResults = for (_ <- 1 to Repeats) yield time { impl.longestSequence(nums) }
    val (durations, results) = durationsAndResults.unzip
    for (result <- results) {
      assertEqual(ExpectedValue, result.value)
      assertEqual(ExpectedSeqSize, result.sequenceSize)
    }
    val totalDuration = durations.sum
    val avgDuration = totalDuration / Repeats

    println("%.3fs".format(avgDuration))
  }

  def time[T](f: => T): (Double, T) = {
    val begin = System.nanoTime()
    val result: T = f
    val end = System.nanoTime()
    val duration = (end - begin) / 1e9
    (duration, result)
  }
}
