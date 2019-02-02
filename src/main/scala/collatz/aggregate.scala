package collatz

import scala.collection.GenSeq

trait BaseAggregateImpl extends RecursiveSeqSizeCollatzImpl {
  protected def longestSeq(values: GenSeq[Long]): Collatz =
    values.aggregate(zero)(makeAndCombine, combine)
}

object AggregateImpl extends BaseAggregateImpl {
  override def longestSequence(values: Seq[Long]): Collatz =
    longestSeq(values)
}

object ParallelAggregateImpl extends BaseAggregateImpl {
  override def longestSequence(values: Seq[Long]): Collatz =
    longestSeq(values.par)
}
