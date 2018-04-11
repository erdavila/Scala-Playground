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

  def addNode(): Node[K, V] = {
    val newNode = new Node[K, V](hashFunction)
    val newToken = generateToken()
    val ringAfter = ring + (newToken -> newNode)

    if (ring.nonEmpty) {
      val rangeBegin = ringAfter.keySet.circular.before(newToken)
      val rangeEnd = newToken
      val assignedNodeBefore = nodeForHash(newToken)
      moveRangeData(rangeBegin, rangeEnd)(assignedNodeBefore, newNode)
    }

    ring = ringAfter

    newNode
  }

  @tailrec
  private def generateToken(): Token = {
    val newToken = Token.generate(random)
    if (ring contains newToken) {
      generateToken()
    } else {
      newToken
    }
  }

  def removeNode(node: Node[K, V]): Unit = {
    val nodeToken = {
      val collected = ring.collectFirst { case (token, `node`) => token }
      require(collected.isDefined, "node not present in the ring")
      collected.head
    }
    val ringAfter = ring - nodeToken

    val rangeBegin = ring.keySet.circular.before(nodeToken)
    val rangeEnd = nodeToken
    val assignedNodeAfter = nodeForHash(nodeToken, ring = ringAfter)
    moveRangeData(rangeBegin, rangeEnd)(node, assignedNodeAfter)

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
