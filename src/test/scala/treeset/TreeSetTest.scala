package treeset

import scala.util.Try

object TreeSetTest {

  def main(args: Array[String]): Unit = {
    testFromSortedDistinctIterator()
    testContains()
    testSize()
    testIsEmpty()
    testAdd()
    testRemove()
    testUnion()
    testIntersect()
    testDifference()
  }

  private def testFromSortedDistinctIterator(): Unit = {
    for {
      size <- 0 to 10
      values = 1 to size
    } {
      val s = TreeSet.fromSortedDistinctIterator(values.iterator)

      assert(s.iterator.toSeq == values.sorted, s"$values -> $s")
      assert(LeftBalance.check(s).isDefined, s"$values -> $s")

      for {
        n <- 1 until size
        modify <- Seq(swap _, repeat _)
      } {
        val vals = modify(values, n)
        val t = Try { TreeSet.fromSortedDistinctIterator(vals.iterator) }
        assert(t.isFailure)
      }
    }

    sealed trait Balance {
      def atLeft: Balance
      def atRight: Balance
      def cmp(l: Int, r: Int): Boolean
      final def check(s: TreeSet[Int]): Option[Int] =
        s match {
          case TreeSet.Empty() => Some(0)
          case TreeSet.Node(left, _, right) =>
            (atLeft.check(left), atRight.check(right)) match {
              case (Some(l), Some(r)) if cmp(l, r) => Some(l + 1)
              case _ => None
            }
        }
    }

    case object LeftBalance extends Balance {
      override def atLeft: Balance = FullBalance
      override def atRight: Balance = LeftBalance
      override def cmp(l: Int, r: Int): Boolean = l >= r
    }

    case object FullBalance extends Balance {
      override def atLeft: Balance = FullBalance
      override def atRight: Balance = FullBalance
      override def cmp(l: Int, r: Int): Boolean = l == r
    }

    def swap(values: IndexedSeq[Int], pos: Int): IndexedSeq[Int] =
      values.take(pos - 1) ++ IndexedSeq(values(pos), values(pos - 1)) ++ values.drop(pos + 1)

    def repeat(values: IndexedSeq[Int], pos: Int): IndexedSeq[Int] =
      values.updated(pos, values(pos - 1))
  }

  private def testContains(): Unit = {
    assert(!TreeSet.empty[Int].contains(7))

    assert(!TreeSet[Int]().contains(7))

    val set = TreeSet(3, 4, 7)
    assert(set.contains(3))
    assert(set.contains(4))
    assert(!set.contains(5))
    assert(!set.contains(6))
    assert(set.contains(7))
  }

  private def testSize(): Unit = {
    assert(TreeSet.empty[Int].size == 0)

    assert(TreeSet[Int]().size == 0)

    val set = TreeSet(3, 4, 7)
    assert(set.size == 3)
  }

  private def testIsEmpty(): Unit = {
    assert(TreeSet.empty[Int].isEmpty)

    assert(TreeSet[Int]().isEmpty)

    val set = TreeSet(3, 4, 7)
    assert(!set.isEmpty)
  }

  private def testAdd(): Unit =
    for {
      max <- 1 to 5
      toAdd <- (1 to max).permutations
    } {
      val s0 = TreeSet.empty[Int]

      toAdd.foldLeft((s0, Set.empty[Int], toAdd.toSet)) { case ((s, added, notAdded), x) =>
        val s2 = s.add(x)

        val newAdded = added + x
        val newNotAdded = notAdded - x

        assert(newAdded.forall(x => s2.contains(x)), s"$toAdd; $x")
        assert(newNotAdded.forall(x => !s2.contains(x)), s"$toAdd; $x")

        (s2, newAdded, newNotAdded)
      }
    }

  private def testRemove(): Unit =
    for {
      max <- 1 to 5
      init <- (1 to max).permutations
      toRemove <- (1 to max).permutations
    } {
      val s0 = TreeSet(init: _*)

      toRemove.foldLeft((s0, Set.empty[Int], init.toSet)) { case ((s, removed, notRemoved), x) =>
        val s2 = s.remove(x)

        val newRemoved = removed + x
        val newNotRemoved = notRemoved - x

        assert(newRemoved.forall(x => !s2.contains(x)), s"$init; $toRemove; $x")
        assert(newNotRemoved.forall(x => s2.contains(x)), s"$init; $toRemove; $x")

        (s2, newRemoved, newNotRemoved)
      }
    }

  private def testUnion(): Unit =
    testBinaryOperation(_ union _, _ union _)("Union", 3)

  private def testIntersect(): Unit =
    testBinaryOperation(_ intersect _, _ intersect _)("Intersect", 2)

  private def testDifference(): Unit =
    testBinaryOperation(_ difference _, _ diff _)("Difference", 2)

  private def testBinaryOperation(
    op: (TreeSet[Int], TreeSet[Int]) => TreeSet[Int],
    opCheck: (Set[Int], Set[Int]) => Set[Int],
  )(opName: String, variations: Int): Unit = {
    val MaxSize = 5
    val MaxValue = 6
    assert(MaxSize <= MaxValue)

    def inits: IndexedSeq[IndexedSeq[Int]] =
      for {
        size <- 1 to MaxSize
        init <- combinations(size, 1 to MaxValue)
      } yield init

    for (variation <- 0 until variations) {
      TreeSet.variation = variation
      TreeSet.instances = 0
      val start = System.nanoTime()

      for {
        initA <- inits
        a = TreeSet(initA: _*)
        initB <- inits
        b = TreeSet(initB: _*)
      } {
        val result = op(a, b)

        assert(result.iterator.toSet == opCheck(initA.toSet, initB.toSet), s"$a $opName $b != $result [$initA, $initB]")
      }

      val end = System.nanoTime()
      println(s"$opName $variation: ${TreeSet.instances} in ${(end - start) / 1000000000.0}s")
    }
  }

  private def combinations[A](n: Int, vals: IndexedSeq[A]): Iterator[IndexedSeq[A]] = {
    val Len = vals.length
    require(n <= Len)

    def generateIdxsSeqs(sz: Int, denied: Set[Int] = Set.empty): Iterator[IndexedSeq[Int]] = {
      def candidates: Iterator[Int] = (0 until Len).iterator.filterNot(denied)

      if (sz > 1) {
        for {
          x <- candidates
          xs <- generateIdxsSeqs(sz - 1, denied + x)
        } yield x +: xs
      } else {
        for (x <- candidates) yield IndexedSeq(x)
      }
    }

    generateIdxsSeqs(n)
      .map { idxs => idxs.map(idx => vals(idx)) }
  }
}
