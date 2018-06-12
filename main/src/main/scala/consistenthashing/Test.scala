package consistenthashing

import consistenthashing.service.Coordinator
import consistenthashing.service.Node
import scala.collection.mutable
import scala.util.Random

class Test(random: Random) {

  private val NumberOfTokensToChoose = Set(20, 30, 40)

  private var replicationFactor = 3
  private val coordinator = new Coordinator[String, String](replicationFactor, _.hashCode, random)
  private val numberOfTokens = mutable.Map.empty[Node[String, String], Int]
  private val entries = mutable.Map.empty[String, String]

  def run(): Unit = {
    addNodes(1).andCheck()
    populateEntries().andCheck()
    addNodes(1).andCheck()
    addNodes(1).andCheck()
    addNodes(1).andCheck()

    addNodes().andCheck()
    populateEntries().andCheck()

    addNodes().andCheck()
    populateEntries().andCheck()

    removeNodes().andCheck()
    changeReplicationFactor().andCheck()
    changeNumberOfTokens().andCheck()
    populateEntries().andCheck()

    showStats()

    removeNodes(numberOfTokens.size - 3).andCheck()
    removeNodes(numberOfTokens.size - 2).andCheck()
    removeNodes(numberOfTokens.size - 1).andCheck()
  }

  private def addNodes(numNodes: Int = 10): Unit =
    for (_ <- 1 to numNodes) {
      val numTokens = chooseOne(NumberOfTokensToChoose)
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

  private def changeReplicationFactor(): Unit = {
    val newReplicationFactor = 2
    coordinator.setReplicationFactor(newReplicationFactor)
    replicationFactor = newReplicationFactor
  }

  private def removeNodes(numNodes: Int = 5): Unit =
    for (node <- choose(numNodes)(numberOfTokens.keys)) {
      coordinator.removeNode(node)
      numberOfTokens.remove(node)
    }

  private def changeNumberOfTokens(): Unit =
    for (node <- choose(10)(numberOfTokens.keys)) {
      val newNumTokens = chooseOne(NumberOfTokensToChoose)
      coordinator.setNumTokens(node, newNumTokens)
    }

  private def choose[A](count: Int)(candidates: Iterable[A]): Iterable[A] =
    random.shuffle(candidates).take(count)

  private implicit class BlockOps[A](f: => A) {
    def andCheck(): Unit = {
      f

      val effectiveReplicationFactor = numberOfTokens.size min replicationFactor

      val totalEntries = numberOfTokens.keysIterator.map(_.entriesCount).sum
      assert(totalEntries == entries.size * effectiveReplicationFactor)

      for {
        (key, value) <- entries
        nodes = coordinator.nodesForKey(key)
        _ = assert(nodes.size == effectiveReplicationFactor)
        node <- nodes
      } {
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
