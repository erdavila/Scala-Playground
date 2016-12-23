package heap

import scala.annotation.tailrec
import scala.collection.GenTraversableOnce
import scala.collection.mutable.ArrayBuffer

object PriorityQueue {
  private[heap] sealed abstract class IndexNotification[T](elemAndIndex: (T, Int)*) extends Traversable[(T, Int)] {
    def foreach[U](f: ((T, Int)) => U) = elemAndIndex.foreach(f)
  }

  private[heap] case class Appended[T](elem: T, index: Int) extends IndexNotification[T](elem -> index)
  private[heap] case class Moved[T](elem: T, toIndex: Int, fromIndex: Int) extends IndexNotification[T](elem -> toIndex)
  private[heap] case class Swapped[T](parent: T, parentIndex: Int, child: T, childIndex: Int) extends IndexNotification[T](parent -> parentIndex, child -> childIndex)
  private[heap] case class Removed[T](elem: T, index: Int) extends IndexNotification[T](elem -> index)

  private[heap] type Notify[T] = PartialFunction[IndexNotification[T], Unit]
}

abstract class PriorityQueue[T](
    initialValues: GenTraversableOnce[T],
    order: Int,
    notifyOpt: Option[PriorityQueue.Notify[T]],
    ord: Ordering[T]) {

  import PriorityQueue._

  private val values = ArrayBuffer.empty[T]
  initialValues foreach put

  def size: Int = values.size
  def isEmpty: Boolean = values.isEmpty
  def nonEmpty: Boolean = values.nonEmpty

  private def lastIndex = values.size - 1

  def put(elem: T): Unit = {
    values.append(elem)
    notify(Appended(elem, lastIndex))

    heapUp(size - 1)
  }

  def first: T = values(0)

  def removeFirst(): T = {
    val first = values(0)
    moveLastTo(0)
    first
  }

  private[heap] def moveLastTo(index: Int): Unit = {
    val lastIdx = lastIndex
    val elem = values.remove(lastIdx)

    if (index == lastIdx) {
      notify(Removed(elem, lastIdx))
    } else {
      assert(index < lastIdx, s"$index >= $lastIdx")
      values(index) = elem
      notify(Moved(elem, index, lastIdx))
      heapDown(index)
      heapUp(index)
    }
  }

  private def heapDown(parentIndex: Int): Unit = {
    val firstChildIndex = order * parentIndex + 1

    (0 until order)
      .map { childNumber => firstChildIndex + childNumber }
      .takeWhile { _ < size }
      .foreach { childIndex =>
        if (rearrange(parentIndex, childIndex)) {
          heapDown(childIndex)
        }
      }
  }

  @tailrec
  private def heapUp(childIndex: Int): Unit =
    if (childIndex > 0) {
      val parentIndex = ((childIndex - 1) / order).toInt
      if (rearrange(parentIndex, childIndex)) {
        heapUp(parentIndex)
      }
    }

  private def rearrange(parentIndex: Int, childIndex: Int): Boolean =
    if (childIndex < size) {
      val parent = values(parentIndex)
      val child = values(childIndex)
      if (ord.lt(child, parent)) {
        val newParent = child
        val newChild = parent
        values(parentIndex) = newParent
        values(childIndex) = newChild
        notify(Swapped(newParent, parentIndex, newChild, childIndex))
        true
      } else false
    } else false

  private def notify(notification: => IndexNotification[T]): Unit =
    notifyOpt foreach { _.lift(notification) }

  /*
   * order = 2:
   *             0
   *          /    \
   *       1           2
   *     /  \         /   \
   *   3     4      5       6
   *  / \   / \    / \     / \
   * 7  8  9  10  11  12  13  14
   *
   * order = 3:
   *             0
   *        /    |    \
   *    1        2          3
   *  / | \    / | \     /  |  \
   * 4  5  6  7  8  9  10  11  12
   */
}

class MinPriorityQueue[T] private[heap] (
    initialValues: GenTraversableOnce[T],
    val order: Int,
    notifyOpt: Option[PriorityQueue.Notify[T]])(implicit ord: Ordering[T])
  extends PriorityQueue(initialValues, order, notifyOpt, ord) {

  def this(initialValues: GenTraversableOnce[T], order: Int = 2)(implicit ord: Ordering[T]) =
    this(initialValues, order, None)(ord)
}

class MaxPriorityQueue[T] private[heap] (
    initialValues: GenTraversableOnce[T],
    val order: Int,
    notifyOpt: Option[PriorityQueue.Notify[T]])(implicit ord: Ordering[T])
  extends PriorityQueue(initialValues, order, notifyOpt, ord.reverse) {

  def this(initialValues: GenTraversableOnce[T], order: Int = 2)(implicit ord: Ordering[T]) =
    this(initialValues, order, None)(ord)
}
