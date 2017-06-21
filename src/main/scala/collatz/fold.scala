package collatz

object FoldImpl extends RecursiveSeqSizeCollatzImpl {
  override def longestSequence(values: Seq[Long]): Collatz =
    values.foldLeft(zero)(makeAndCombine)
}

object CachedFoldImpl extends CachedSeqSizeCollatzImpl {
  override def longestSequence(values: Seq[Long]): Collatz = {
    val seqSizeCache: SeqSizeCache = Map.empty
    val (_, longest) = values.foldLeft((seqSizeCache, zero)) { case ((seqSizeCache, longest),  n) =>
      val (seqSz, newSeqSizeCache) = seqSize(n, seqSizeCache)
      val collatz = Collatz(n, seqSz)
      val newLongest = combine(longest, collatz)
      (newSeqSizeCache, newLongest)
    }

    longest
  }
}

object MutablyCachedFoldImpl extends MutablyCachedSeqSizeCollatzImpl {
  override def longestSequence(values: Seq[Long]): Collatz = {
    val cache: SeqSizeCache = MutableCache(values.size, 1L -> 1)
    values.foldLeft(zero) { (longest,  n) =>
      val seqSz = seqSize(n, cache)
      val collatz = Collatz(n, seqSz)
      combine(longest, collatz)
    }
  }
}
