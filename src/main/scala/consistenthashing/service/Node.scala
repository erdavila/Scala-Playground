package consistenthashing.service

import scala.collection.mutable

class Node[K, V](hashFunction: K => Hash) {

  private val entries = mutable.SortedMap.empty[Hash, mutable.Map[K, V]]

  def entriesCount: Int = entries.values.map(_.size).sum

  def get(key: K): Option[V] = {
    val hash = hashFunction(key)
    for {
      values <- entries.get(hash)
      value <- values.get(key)
    } yield value
  }

  private[service] def put(hash: Hash, key: K, value: V): Unit =
    entries.getOrElseUpdate(hash, mutable.Map.empty).put(key, value)

  private[service] def remove(hash: Hash, key: K): Unit = {
    val values = entries(hash)
    values.remove(key)
    if (values.isEmpty) {
      entries.remove(hash)
    }
  }

  private[service] def getDataRange(rangeBegin: Token, rangeEnd: Token): Map[Hash, mutable.Map[K, V]] =
    if (rangeBegin < rangeEnd) {
      entries.after(rangeBegin).to(rangeEnd).toMap
    } else {
      val after = entries.after(rangeBegin)
      val to = entries.to(rangeEnd)
      (after ++ to).toMap
    }

  private[service] def removeHashes(hashes: Set[Hash]): Unit =
    entries --= hashes

  private[service] def addData(data: Map[Hash, mutable.Map[K, V]]): Unit =
    entries ++= data

  implicit class SortedMapOps[A, B](sortedMap: mutable.SortedMap[A, B]) {
    def after(after: A): mutable.SortedMap[A, B] =
      sortedMap.from(after).dropWhile { case (key, _) => key == after }
  }
}

object Node {

  def main(args: Array[String]): Unit = {
    getDataRange()
    println("Node OK")
  }

  private def getDataRange(): Unit = {
    val node = new Node[String, String](_.toInt)
    val dummyValue = mutable.Map.empty[String, String]
    node.entries.put(10, dummyValue)
    node.entries.put(20, dummyValue)
    node.entries.put(30, dummyValue)

    assert(node.getDataRange(10, 20).keySet == Set(20))
    assert(node.getDataRange(10, 30).keySet == Set(20, 30))
    assert(node.getDataRange(20, 30).keySet == Set(30))
    assert(node.getDataRange(20, 10).keySet == Set(30, 10))
    assert(node.getDataRange(30, 10).keySet == Set(10))
    assert(node.getDataRange(30, 20).keySet == Set(10, 20))
  }
}
