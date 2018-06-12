package collatz

object ProceduralImpl extends RecursiveSeqSizeCollatzImpl {
  override def longestSequence(values: Seq[Long]): Collatz = {
    var longest = zero
    for (n <- values) {
      longest = makeAndCombine(longest, n)
    }
    longest
  }
}

object CachedProceduralImpl extends MutablyCachedSeqSizeCollatzImpl {
  override def longestSequence(values: Seq[Long]): Collatz = {
    val cache: SeqSizeCache = MutableCache(values.size, 1L -> 1)

    var longest = zero
    for (n <- values) {
      val size = seqSize(n, cache)
      val collatz = Collatz(n, size)
      longest = combine(longest, collatz)
    }
    longest
  }
}
