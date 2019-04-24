package treeset

import scala.annotation.tailrec


sealed trait TreeSet[A] {
  def size: Int
  def isEmpty: Boolean

  def contains(x: A): Boolean

  def add(x: A): TreeSet[A]
  def remove(x: A): TreeSet[A]

  def union(that: TreeSet[A]): TreeSet[A]
  def intersect(that: TreeSet[A]): TreeSet[A]
  def difference(that: TreeSet[A]): TreeSet[A]

  def iterator: Iterator[A]
}


object TreeSet {
  def empty[A: Ordering]: TreeSet[A] = Empty()

  def apply[A: Ordering](xs: A*): TreeSet[A] =
    if (xs.isEmpty) {
      empty
    } else {
      apply(xs.tail: _*).add(xs.head)
    }

  def fromSortedDistinctIterator[A](it: Iterator[A])(implicit ord: Ordering[A]): TreeSet[A] = {
    import ord._

    def make(idxIt: BufferedIterator[(A, Int)])(maxAtLeft: Option[A]): (TreeSet[A], Option[A]) =
      makeWithLeft(Empty[A](), idxIt, 1)(maxAtLeft)

    @tailrec
    def makeWithLeft(left: TreeSet[A], idxIt: BufferedIterator[(A, Int)], n: Int)(maxAtLeft: Option[A]): (TreeSet[A], Option[A]) =
      idxIt.headOption match {
        case Some((_, firstIdx)) =>
          val (it, restIt) = idxIt.span { case (_, idx) => idx < firstIdx + n}
          val (value, _) = it.next()
          require(maxAtLeft.forall(_ < value), "Iterator is not sorted or not unique")
          val (right, maxAtRight) = make(it.buffered)(Some(value))

          val s = Node[A](left, value, right)

          val newMaxAtRight = maxAtRight orElse Some(value)
          if (restIt.isEmpty) {
            (s, newMaxAtRight)
          } else {
            makeWithLeft(s, restIt.buffered, 2 * n)(newMaxAtRight)
          }
        case None =>
          (left, maxAtLeft)
      }

    val (s, _) = make(it.zipWithIndex.buffered)(None)
    s
  }

  case class Empty[A: Ordering]() extends TreeSet[A] {
    override def size: Int = 0
    override def isEmpty: Boolean = true

    override def contains(x: A): Boolean = false

    override def add(x: A): TreeSet[A] = Node(this, x, this)
    override def remove(x: A): TreeSet[A] = this

    override def union(that: TreeSet[A]): TreeSet[A] = that
    override def intersect(that: TreeSet[A]): TreeSet[A] = this
    override def difference(that: TreeSet[A]): TreeSet[A] = this

    override def iterator: Iterator[A] = Iterator.empty
  }

  private sealed trait Order
  private case object LT extends Order
  private case object EQ extends Order
  private case object GT extends Order


  var variation: Int = 0
  var instances: Int = 0
  case class Node[A](left: TreeSet[A], value: A, right: TreeSet[A])(implicit ord: Ordering[A]) extends TreeSet[A] {
    import ord._

    val size: Int = left.size + 1 + right.size
    instances += 1

    override def isEmpty: Boolean = false

    override def contains(x: A): Boolean =
      compareToValue(x) match {
        case LT => left.contains(x)
        case EQ => true
        case GT => right.contains(x)
      }

    override def add(x: A): TreeSet[A] =
      compareToValue(x) match {
        case LT =>
          val l = left.add(x)
          if (l eq left) this else copy(left = l)
        case EQ =>
          this
        case GT =>
          val r = right.add(x)
          if (r eq right) this else copy(right = r)
      }

    override def remove(x: A): TreeSet[A] =
      compareToValue(x) match {
        case LT =>
          val l = left.remove(x)
          if (l eq left) this else copy(left = l)
        case EQ =>
          (left, right) match {
            case (l@Empty(), Empty()) => l
            case (_, Empty()) => left
            case (Empty(), _) => right
            case (lNode@Node(_, _, _), rNode@Node(_, _, _)) =>
              if (lNode.size < rNode.size) {
                val (y, l) = lNode.removeLargest()
                Node(l, y, right)
              } else {
                val (y, r) = rNode.removeSmallest()
                Node(left, y, r)
              }
          }
        case GT =>
          val r = right.remove(x)
          if (r eq right) this else copy(right = r)
      }

    private def compareToValue(x: A): Order =
      compare(x, value)

    private def removeLargest(): (A, TreeSet[A]) =
      right match {
        case Empty() =>
          (value, left)
        case right@Node(_, _, _) =>
          val (x, r) = right.removeLargest()
          (x, Node(left, value, r))
      }

    private def removeSmallest(): (A, TreeSet[A]) =
      left match {
        case Empty() =>
          (value, right)
        case left@Node(_, _, _) =>
          val (x, l) = left.removeSmallest()
          (x, Node(l, value, right))
      }

    override def union(that: TreeSet[A]): TreeSet[A] =
      variation match {
        case 1 =>
          val it = join(this.iterator, that.iterator).collect { case (v, _) => v }
          TreeSet.fromSortedDistinctIterator(it)
        case 2 =>
          val (a, b) = bySize(that)
          b match {
            case b@Node(_, _, _) => b.unionWith(a.iterator)
            case Empty() => a
          }
        case _ =>
          val (a, b) = bySize(that)
          a.iterator.foldLeft(b) { (s, x) => s.add(x) }
      }

    private def unionWith(it0: Iterator[A]): TreeSet[A] = {
      val (itLT, itGE) = it0.span(_ < value)
      val l = left match {
        case l@Node(_, _, _) if itLT.nonEmpty => l.unionWith(itLT)
        case Empty() if itLT.nonEmpty => fromSortedDistinctIterator(itLT)
        case _ => left
      }

      val itGT = itGE.dropWhile(_ == value)
      val r = right match {
        case r@Node(_, _, _) if itGT.nonEmpty => r.unionWith(itGT)
        case Empty() if itGT.nonEmpty => fromSortedDistinctIterator(itGT)
        case _ => right
      }

      if ((l eq left) && (r eq right)) {
        this
      } else {
        Node(l, value, r)
      }
    }

    private def compare(x: A, y: A): Order =
      if (x < y) LT
      else if (x > y) GT
      else EQ

    override def intersect(that: TreeSet[A]): TreeSet[A] =
      variation match {
        case 1 =>
          val it = join(this.iterator, that.iterator).collect { case (v, Both) => v }
          TreeSet.fromSortedDistinctIterator(it)
        case _ =>
          val (a, b) = bySize(that)
          a.iterator.foldLeft(empty) { (s, x) => if (b contains x) s.add(x) else s }
      }

    override def difference(that: TreeSet[A]): TreeSet[A] =
      variation match {
        case 1 =>
          val it = join(this.iterator, that.iterator).collect { case (v, LeftOnly) => v }
          TreeSet.fromSortedDistinctIterator(it)
        case _ =>
          that.iterator.foldLeft(this: TreeSet[A]) { (s, x) => s.remove(x) }
      }

    private def bySize(that: TreeSet[A]): (TreeSet[A], TreeSet[A]) = {
      val Seq(a, b) = Seq(this, that).sortBy(_.size)
      (a, b)
    }

    override def iterator: Iterator[A] =
      Iterator.single(())
        .flatMap { _ => left.iterator ++ Iterator.single(value) ++ right.iterator }


    private sealed trait ElementSource
    private case object LeftOnly extends ElementSource
    private case object RightOnly extends ElementSource
    private case object Both extends ElementSource
    private def join(itL: Iterator[A], itR: Iterator[A]): Iterator[(A, ElementSource)] =
      new Iterator[(A, ElementSource)] {
        private val l = itL.buffered
        private val r = itR.buffered

        override def hasNext: Boolean =
          l.hasNext || r.hasNext

        override def next(): (A, ElementSource) =
          (l.headOption, r.headOption) match {
            case (Some(lVal), rOpt) if rOpt.forall(lVal < _) => l.next(); (lVal, LeftOnly)
            case (lOpt, Some(rVal)) if lOpt.forall(_ > rVal) => r.next(); (rVal, RightOnly)
            case (Some(lVal), Some(_)) => l.next(); r.next(); (lVal, Both)
          }
      }
  }
}
