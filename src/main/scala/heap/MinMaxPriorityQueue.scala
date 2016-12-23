package heap

import scala.collection.GenTraversableOnce

object MinMaxPriorityQueue {
  private case class Node[T](value: T, var minQIndex: Int = -1, var maxQIndex: Int = -1)
}

class MinMaxPriorityQueue[T](initialValues: GenTraversableOnce[T], order: Int)(implicit ord: Ordering[T]) {
  import MinMaxPriorityQueue._
  import PriorityQueue._

  private type NodeT = Node[T]

  private val (minQ, maxQ) = {

    def notify(updateIndex: (NodeT, Int) => Unit): Notify[NodeT] = {
      case idxNotif =>
        for ((elem, index) <- idxNotif) {
          updateIndex(elem, index)
        }
    }

    val notifyMinQ: Notify[NodeT] = notify { (node, idx) => node.minQIndex = idx }
    val notifyMaxQ: Notify[NodeT] = notify { (node, idx) => node.maxQIndex = idx }

    implicit val nodeOrdering = new Ordering[NodeT] {
      def compare(x: NodeT, y: NodeT) = ord.compare(x.value, y.value)
    }

    val minQ = new MinPriorityQueue[NodeT](Traversable.empty, order, Some(notifyMinQ))
    val maxQ = new MaxPriorityQueue[NodeT](Traversable.empty, order, Some(notifyMaxQ))

    (minQ, maxQ)
  }
  initialValues foreach put

  def put(elem: T): Unit = {
    val node = Node(elem)
    minQ.put(node)
    maxQ.put(node)
  }

  def min: T = minQ.first.value
  def max: T = maxQ.first.value

  def removeMin(): T = {
    val nodeMin = minQ.removeFirst()
    maxQ.moveLastTo(nodeMin.maxQIndex)
    nodeMin.value
  }

  def removeMax(): T = {
    val nodeMax = maxQ.removeFirst()
    minQ.moveLastTo(nodeMax.minQIndex)
    nodeMax.value
  }

  def size: Int = minQ.size
  def isEmpty: Boolean = minQ.isEmpty
  def nonEmpty: Boolean = minQ.nonEmpty
}
