package consistenthashing.service

import CircularSet.Ops
import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.collection.mutable
import scala.util.Random

class Coordinator[K, V](private var replicationFactor: Int, hashFunction: K => Hash, random: Random = Random) {

  require(replicationFactor >= 1)

  private case class AssignedNodes(main: Node[K, V], all: Set[Node[K, V]])

  private var ring = SortedMap.empty[Token, AssignedNodes]

  def put(key: K, value: V): Unit = {
    val (hash, nodes) = hashAndNodesForKey(key)
    for (node <- nodes) {
      node.put(hash, key, value)
    }
  }

  def remove(key: K): Unit = {
    val (hash, nodes) = hashAndNodesForKey(key)
    for (node <- nodes) {
      node.remove(hash, key)
    }
  }

  def nodesForKey(key: K): Set[Node[K, V]] = {
    val (_, nodes) = hashAndNodesForKey(key)
    nodes
  }

  private def hashAndNodesForKey(key: K): (Hash, Set[Node[K, V]]) = {
    require(ring.nonEmpty)
    val hash = hashFunction(key)
    val nodes = nodesForHash(hash)
    (hash, nodes)
  }

  def addNode(numTokens: Int): Node[K, V] = {
    require(numTokens >= 1)

    val newNode = new Node[K, V](hashFunction)
    addTokens(newNode, numTokens)

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
      val collected = ring.collect { case (token, AssignedNodes(`node`, _)) => token }
      require(collected.nonEmpty, "node not present in the ring")
      collected.toSet
    }

    removeTokens(node, nodeTokens)
  }

  def setNumTokens(node: Node[K, V], newNumTokens: Int): Unit = {
    require(newNumTokens >= 1)

    val tokens = ring.collect { case (token, AssignedNodes(`node`, _)) => token }

    if (newNumTokens > tokens.size) {
      addTokens(node, newNumTokens - tokens.size)
    } else if (newNumTokens < tokens.size) {
      val tokensToRemove = random.shuffle(tokens).take(tokens.size - newNumTokens)
      removeTokens(node, tokensToRemove.toSet)
    }
  }

  private def addTokens(node: Node[K, V], numNewTokens: Int): Unit = {
    val newTokens = generateTokens(numNewTokens)
    val mainNodesAfter = ring.mapValues(_.main) ++ newTokens.map { _ -> node }
    val ringAfter = makeRing(mainNodesAfter)

    if (ring.nonEmpty) {
      distributeReplicas(ringAfter, ringAfter)
    }

    ring = ringAfter
  }

  private def removeTokens(node: Node[K, V], tokens: Set[Token]): Unit = {
    val mainNodesAfter = (ring -- tokens).mapValues(_.main)
    val ringAfter = makeRing(mainNodesAfter)

    distributeReplicas(ring, ringAfter)

    ring = ringAfter
  }

  def setReplicationFactor(newReplicationFactor: Int): Unit = {
    require(newReplicationFactor >= 1)

    if (newReplicationFactor != replicationFactor) {
      val mainNodes = ring.mapValues(_.main)
      val ringAfter = makeRing(mainNodes, newReplicationFactor)

      distributeReplicas(ring, ringAfter)

      replicationFactor = newReplicationFactor
      ring = ringAfter
    }
  }

  private def makeRing(mainNodes: SortedMap[Token, Node[K, V]], replicationFactor: Int = this.replicationFactor): SortedMap[Token, AssignedNodes] =
    for ((token, mainNode) <- mainNodes)
    yield {
      val allAssignedNodes = mutable.Set.empty[Node[K, V]]
      val tokens = mainNodes.keySet.circular.iteratorAfterOrAt(token)
      while (allAssignedNodes.size < replicationFactor  &&  tokens.hasNext) {
        val assignedNodeToken = tokens.next()
        val assignedNode = mainNodes(assignedNodeToken)
        allAssignedNodes += assignedNode
      }
      token -> AssignedNodes(mainNode, allAssignedNodes.toSet)
    }

  private def distributeReplicas(iterationRing: SortedMap[Token, AssignedNodes], ringAfter: SortedMap[Token, AssignedNodes]): Unit =
    for ((token, assignedNodes) <- iterationRing) {
      val rangeBegin = iterationRing.keySet.circular.before(token)
      val rangeEnd = token
      val assignedNodesBefore = nodesForHash(token, ring = ring)
      val assignedNodesAfter = nodesForHash(token, ring = ringAfter)
      moveRangeData(rangeBegin, rangeEnd)(assignedNodesBefore, assignedNodesAfter)
    }

  private def nodesForHash(hash: Hash, ring: SortedMap[Token, AssignedNodes] = this.ring): Set[Node[K, V]] = {
    val token = ring.keySet.circular.afterOrAt(hash)
    val nodes = ring(token).all
    nodes
  }

  private def moveRangeData(rangeBegin: Token, rangeEnd: Token)(fromNodes: Set[Node[K, V]], toNodes: Set[Node[K, V]]): Unit =
    if (fromNodes != toNodes) {
      val data = fromNodes.head.getDataRange(rangeBegin, rangeEnd)

      val newNodes = toNodes -- fromNodes
      for (node <- newNodes) {
        node.addData(data)
      }

      val unassignedNodes = fromNodes -- toNodes
      for (node <- unassignedNodes) {
        node.removeHashes(data.keySet)
      }
    }
}
