package collatz

import scala.collection.mutable

class MutableCache[K, V](size: Int) extends mutable.HashMap[K, V] {
  override def initialSize = size
  threshold = initialSize + 1
}

object MutableCache {
  def apply[K, V](size: Int, pairs: (K, V)*): MutableCache[K, V] = {
    val cache = new MutableCache[K, V](size)
    cache ++= pairs
    cache
  }
}
