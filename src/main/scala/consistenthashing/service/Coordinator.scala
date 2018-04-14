package consistenthashing.service

import CircularSet.Ops
import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.util.Random

class Coordinator[K, V](hashFunction: K => Hash, random: Random = Random) {

  private var ring = SortedMap.empty[Token, Node[K, V]]

  def put(key: K, value: V): Unit = {
    val (hash, node) = hashAndNodeForKey(key)
    node.put(hash, key, value)
  }

  def remove(key: K): Unit = {
    val (hash, node) = hashAndNodeForKey(key)
    node.remove(hash, key)
  }

  def nodeForKey(key: K): Node[K, V] = {
    val (_, node) = hashAndNodeForKey(key)
    node
  }

  private def hashAndNodeForKey(key: K): (Hash, Node[K, V]) = {
    require(ring.nonEmpty)
    val hash = hashFunction(key)
    val node = nodeForHash(hash)
    (hash, node)
  }

  def addNode(numTokens: Int): Node[K, V] = {
    require(numTokens >= 1)

    val newNode = new Node[K, V](hashFunction)
    val newTokens = generateTokens(numTokens)
    val ringAfter = ring ++ newTokens.map { _ -> newNode }

    if (ring.nonEmpty) {
      for (newToken <- newTokens) {
        val rangeBegin = ringAfter.keySet.circular.before(newToken)
        val rangeEnd = newToken
        val assignedNodeBefore = nodeForHash(newToken)
        moveRangeData(rangeBegin, rangeEnd)(assignedNodeBefore, newNode)
      }
    }

    ring = ringAfter

    newNode
  }

  private def generateTokens(numTokens: Int): Set[Token] = {
    @tailrec
    def loop(newTokens: Set[Token]): Set[Token] =
      if (newTokens.size < numTokens) {
        val newToken = Token.generate(random)
        if (ring contains newToken) {
          loop(newTokens)
        } else {
          loop(newTokens + newToken)
        }
      } else {
        newTokens
      }

    loop(Set.empty)
  }

  def removeNode(node: Node[K, V]): Unit = {
    val nodeTokens = {
      val collected = ring.collect { case (token, `node`) => token }
      require(collected.nonEmpty, "node not present in the ring")
      collected.toSet
    }
    val ringAfter = ring -- nodeTokens

    for (token <- nodeTokens) {
      val rangeBegin = ring.keySet.circular.before(token)
      val rangeEnd = token
      val assignedNodeAfter = nodeForHash(token, ring = ringAfter)
      moveRangeData(rangeBegin, rangeEnd)(node, assignedNodeAfter)
    }

    ring = ringAfter
  }

  private def nodeForHash(hash: Hash, ring: SortedMap[Token, Node[K, V]] = this.ring): Node[K, V] = {
    val token = ring.keySet.circular.afterOrAt(hash)
    val node = ring(token)
    node
  }

  private def moveRangeData(rangeBegin: Token, rangeEnd: Token)(fromNode: Node[K, V], toNode: Node[K, V]): Unit = {
    val data = fromNode.getDataRange(rangeBegin, rangeEnd)
    fromNode.removeHashes(data.keySet)
    toNode.addData(data)
  }
}
