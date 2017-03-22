package faísca.aggregations

class Aggregations[T] {

  def count(): Long =
    aggregate(0L)
      { (count, _) => count + 1 }
      { _ + _ }
      { count => count }

  def min()(implicit ord: Ordering[T]): Option[T] =
    aggregate(None: Option[T])
      { (min, elem) => min map { ord.min(_, elem) } orElse Some(elem) }
      { _ orElse _ }
      { elem => elem }

  def max()(implicit ord: Ordering[T]): Option[T] =
    aggregate(None: Option[T])
      { (max, elem) => max map { ord.max(_, elem) } orElse Some(elem) }
      { _ orElse _ }
      { elem => elem }

  def sum()(implicit num: Numeric[T]): T =
    aggregate(num.zero)
      { (partial, elem) => num.plus(partial, elem) }
      { num.plus(_, _) }
      { identity _ }

  def avg()(implicit num: Numeric[T]): Option[Double] =
    aggregate((num.zero, 0L))
      { case ((partial, count), elem) => (num.plus(partial, elem), 1) }
      { case ((partial1, count1), (partial2, count2)) => (num.plus(partial1, partial2), count1 + count2) }
      { case (sum, count) => if (count > 0) Some(num.toDouble(sum) / count) else None }

  def stdDev()(implicit num: Numeric[T]): Option[T] = ???
  def stdDevPop()(implicit num: Numeric[T]): Option[T] = ???
  def variance()(implicit num: Numeric[T]): Option[T] = ???
  def variancePop()(implicit num: Numeric[T]): Option[T] = ???

  def aggregate[A, B](z: A)(foldF: (A, T) => A)(reduceF: (A, A) => A)(finalizeF: A => B): B = ???
}
