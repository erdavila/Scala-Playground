package heap

import PriorityQueue._
import scala.util.Random

object PriorityQueueTest extends App {
  val N = 1000
  val OrderRange = 1 to 10

  val values = Seq.fill(N) { Random.nextInt() }
  val sortedValues = values.sorted

  for (order <- OrderRange) {
    var swapCount = 0
    val notify: Notify[Int] = { case Swapped(_, _, _, _) => swapCount += 1 }
    val prioQ = new MinPriorityQueue(values, order = order, notifyOpt = Some(notify))
    println("order = " + order)
    println("swapCount = " + swapCount)
    swapCount = 0
    sortedValues foreach { value =>
      assert(prioQ.nonEmpty)
      assert(prioQ.first == value)
      assert(prioQ.removeFirst() == value)
    }
    assert(prioQ.isEmpty)
    println("swapCount = " + swapCount)
    println()
  }

  println("OK")
}
