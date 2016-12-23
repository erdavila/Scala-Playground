package heap

import scala.util.Random

object MinMaxPriorityQueueTest extends App {
  val N = 1000
  val OrderRange = 1 to 10

  val Values = Seq.fill(N) { Random.nextInt(100) }
  val SortedValues = Values.sorted

  for (order <- OrderRange) {
    val prioQ = new MinMaxPriorityQueue(Values, order = order)
    println("order = " + order)
    var sortedValues = SortedValues

    def assertBasics() = {
      assert(prioQ.nonEmpty)
      assert(prioQ.min == sortedValues.head)
      assert(prioQ.max == sortedValues.last)
      assert(prioQ.size == sortedValues.size)
    }

    do {
      assertBasics()
      assert(prioQ.removeMin() == sortedValues.head)
      sortedValues = sortedValues.tail

      if(sortedValues.nonEmpty) {
        assertBasics()
        assert(prioQ.removeMax() == sortedValues.last)
        sortedValues = sortedValues.init
      }
    } while(sortedValues.nonEmpty)
    assert(prioQ.isEmpty)
  }
}
