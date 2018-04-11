package consistenthashing

import consistenthashing.service.Coordinator
import consistenthashing.service.Node
import scala.collection.mutable
import scala.util.Random

class Test(random: Random) {

  private val coordinator = new Coordinator[String, String](_.hashCode, random)
  private val nodes = mutable.Set.empty[Node[String, String]]
  private val entries = mutable.Map.empty[String, String]

  def run(): Unit = {
    addNodes().andCheck()
    populateEntries().andCheck()

    addNodes().andCheck()
    populateEntries().andCheck()

    removeNodes().andCheck()
    populateEntries().andCheck()
  }

  private def addNodes(): Unit =
    for (_ <- 1 to 10) {
      val node = coordinator.addNode()
      nodes.add(node)
    }

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
    for (node <- choose(5)(nodes)) {
      coordinator.removeNode(node)
      nodes.remove(node)
    }

  private def choose[A](count: Int)(candidates: Iterable[A]): Iterable[A] =
    random.shuffle(candidates).take(count)

  private implicit class BlockOps[A](f: => A) {
    def andCheck(): Unit = {
      f

      val totalEntries = nodes.toSeq.map(_.entriesCount).sum
      assert(totalEntries == entries.size)

      for ((key, value) <- entries) {
        val node = coordinator.nodeForKey(key)
        val storedValue = node.get(key)
        assert(storedValue == Some(value))
      }
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
