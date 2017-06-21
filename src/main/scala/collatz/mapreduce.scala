package collatz

import scala.collection.GenSeq

trait BaseMapReduceImpl extends RecursiveSeqSizeCollatzImpl {
  def longestSeq(values: GenSeq[Long]): Collatz =
    values.map(make).reduce(combine)
}

object MapReduceImpl extends BaseMapReduceImpl {
  override def longestSequence(values: Seq[Long]): Collatz =
    longestSeq(values)
}

object ParallelMapReduceImpl extends BaseMapReduceImpl {
  override def longestSequence(values: Seq[Long]): Collatz =
    longestSeq(values.par)
}
