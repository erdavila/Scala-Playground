package consistenthashing

import consistenthashing.service.Coordinator
import consistenthashing.service.Node
import scala.collection.mutable
import scala.util.Random

class Test(random: Random) {

  private val coordinator = new Coordinator[String, String](_.hashCode, random)
  private val numberOfTokens = mutable.Map.empty[Node[String, String], Int]
  private val entries = mutable.Map.empty[String, String]

  def run(): Unit = {
    addNodes().andCheck()
    populateEntries().andCheck()

    addNodes().andCheck()
    populateEntries().andCheck()

    removeNodes().andCheck()
    populateEntries().andCheck()

    showStats()
  }

  private def addNodes(): Unit =
    for (_ <- 1 to 10) {
      val numTokens = chooseOne(Seq(20, 30, 40))
      val node = coordinator.addNode(numTokens)
      numberOfTokens.put(node, numTokens)
    }

  private def chooseOne[A](candidates: Iterable[A]): A =
    choose(1)(candidates).head

  private def populateEntries(): Unit =
    for (i <- 1 to 1000) {
      val key = randomString()
      val value = randomString()
      coordinator.put(key, value)
      entries.put(key, value)

      if (i % 50 == 0) {
        for (key <- choose(5)(entries.keys)) {
          coordinator.remove(key)
          entries.remove(key)
        }
      }
    }

  private def randomString(): String =
    Seq.fill(10)(random.nextPrintableChar()).mkString

  private def removeNodes(): Unit =
    for (node <- choose(5)(numberOfTokens.keys)) {
      coordinator.removeNode(node)
      numberOfTokens.remove(node)
    }

  private def choose[A](count: Int)(candidates: Iterable[A]): Iterable[A] =
    random.shuffle(candidates).take(count)

  private implicit class BlockOps[A](f: => A) {
    def andCheck(): Unit = {
      f

      val totalEntries = numberOfTokens.keysIterator.map(_.entriesCount).sum
      assert(totalEntries == entries.size)

      for ((key, value) <- entries) {
        val node = coordinator.nodeForKey(key)
        val storedValue = node.get(key)
        assert(storedValue == Some(value))
      }
    }
  }

  private def showStats(): Unit = {
    val nodesByNumTokens =
      numberOfTokens
        .groupBy { case (node, numTokens) => numTokens }
        .mapValues { _.keySet }
        .toSeq
        .sortBy { case (numTokens, nodes) => numTokens }

    println(Seq("numTokens", "numNodes", "avgEntries").mkString("  "))
    for ((numTokens, nodes) <- nodesByNumTokens) {
      val totalEntries = nodes.map { _.entriesCount }.sum
      printf("%9d  %8d  %10.2f\n",
        numTokens,
        nodes.size,
        totalEntries / nodes.size.toDouble
      )
    }
  }
}

object Test {

  def main(args: Array[String]): Unit = {
    val seed = Random.nextLong()
    println("Random seed: " + seed)
    val random = new Random(seed)

    for (i <- 1 to 10) {
      println(i)
      val test = new Test(random)
      test.run()
    }
    println("Test OK")
  }
}
