package heap

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object PriorityQueueTest extends App {
  val values = ArrayBuffer.empty[Int]

  (1 to 1000) foreach { _ =>
    val value = Random.nextInt()
    values.append(value)
  }
  val sortedValues = values.sorted

  for (order <- 1 to 10) {
    val prioQ = new MinPriorityQueue(values, order = order)
    println("order = " + order)
    println("swapCount = " + prioQ.swapCount)
    prioQ.swapCount = 0
    sortedValues foreach { value =>
      assert(prioQ.nonEmpty)
      assert(prioQ.first == value)
      assert(prioQ.removeFirst() == value)
    }
    assert(prioQ.isEmpty)
    println("swapCount = " + prioQ.swapCount)
    println()
  }

  println("OK")
}
