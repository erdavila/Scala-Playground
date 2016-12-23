package heap

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

abstract class PriorityQueue[T](initialValues: Iterable[T], order: Int, ord: Ordering[T]) {
  protected[heap] var swapCount = 0
  private val values = ArrayBuffer.empty[T]
  initialValues foreach put

  def put(x: T): Unit = {
    values.append(x)
    heapUp(size - 1)
  }

  def first: T = values(0)

  def removeFirst(): T = {
    val first = values(0)
    val last = values.remove(values.size - 1)
    if (values.nonEmpty) {
      values(0) = last
      heapDown()
    }
    first
  }

  def size: Int = values.size
  def isEmpty: Boolean = values.isEmpty
  def nonEmpty: Boolean = values.nonEmpty

  private def heapDown(parentIndex: Int = 0): Unit = {
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
      val parentValue = values(parentIndex)
      val childValue = values(childIndex)
      if (ord.gt(parentValue, childValue)) {
        values(parentIndex) = childValue
        values(childIndex) = parentValue
        swapCount += 1
        true
      } else false
    } else false

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

class MinPriorityQueue[T](initialValues: Iterable[T], val order: Int = 2)(implicit ord: Ordering[T])
    extends PriorityQueue(initialValues, order, ord)

class MaxPriorityQueue[T](initialValues: Iterable[T], val order: Int = 2)(implicit ord: Ordering[T])
    extends PriorityQueue(initialValues, order, ord.reverse)
