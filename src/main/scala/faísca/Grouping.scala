package faísca

trait Column[U]

class Grouping[T] {
  def count(): Column[Long] =
    Aggregator(0L) { _ => 1L } { _ + _ }

  def sum[U](f: T => U)(implicit num: Numeric[U]): Column[U] =
    Aggregator(num.zero)(f) { num.plus(_, _) }

  def avg(f: T => Double): Column[Option[Double]] =
    Aggregator((0.0, 0L)) { t => (f(t), 1L) } ({ case ((s1, c1), (s2, c2)) => (s1 + s2, c1 + c2) }, { case (_, 0) => None; case (s, c) => Some(s / c) })

  def stddevPop(f: T => Double): Column[Option[Double]] =
    stddev(f)

  def stddevSamp(f: T => Double): Column[Option[Double]] =
    stddev(f, -1)

  private def stddev(f: T => Double, d: Long = 0L): Column[Option[Double]] =
    TwoStageAggregator[
        (Seq[Double], Double, Long), // LS - Local State: local values, local sum, local count
        (Double, Long),              // PSS - Partial Shared State: local sum, local count
        Option[Double],              // SS - Shared State: global average option
        (Double, Long),              // PR - Partial Result: sum of squares of (difference of each local value to global average), local count
        Option[Double]               // R - Result: standard deviation option
      ](
        (Seq.empty, 0.0, 0L)  // LS
      ) { t =>
        val x = f(t)
        (Seq(x), x, 1L)  // LS
      } {
        case ((seq1, sum1, count1), (seq2, sum2, count2)) => (seq1 ++ seq2, sum1 + sum2, count1 + count2)  // LS
      } (
        { case (_, sum, count) => (sum, count) },  // PSS
        { psss => average(psss) },  // SS
        { case ((seq, _, count), avgOpt) =>
          avgOpt map { avg =>
            val sum = seq.map { x =>
              val y = x - avg
              y * y
            }.sum
            (sum, count)
          } getOrElse (0.0, 0L)  // PR
        },
        { prs => average(prs, d) map { math.sqrt } }  // R
      )

  private def average(pairs: Iterable[(Double, Long)], d: Long = 0L): Option[Double] = {
    val (sum, count) = pairs.fold((0.0, 0L)) { case ((sum1, count1), (sum2, count2)) => (sum1 + sum2, count1 + count2) }
    if (count == 0) None else Some(sum / (count + d))
  }

  def min[U](f: T => U)(implicit ord: Ordering[U]): Column[Option[U]] =
    Aggregator(None: Option[U]) { t => Some(f(t)) } { (xOpt, yOpt) => for (x <- xOpt orElse yOpt; y <- yOpt orElse xOpt) yield ord.min(x, y) }

  def max[U](f: T => U)(implicit ord: Ordering[U]): Column[Option[U]] =
    min(f)(ord.reverse)

  case class Aggregator[V, W](init: V)(map: T => V)(reduce: (V, V) => V, finish: V => W = { u: V => u }) extends Column[W]

  case class TwoStageAggregator[
      LS,  // Local State
      PSS, // Partial Shared State
      SS,  // Shared State
      PR,  // Partial Result
      R    // Result
    ]
    (init: LS)
    (map: T => LS)
    (reduce: (LS, LS) => LS)
    (
      share: LS => PSS,
      combine: Iterable[PSS] => SS,
      result: (LS, SS) => PR,
      finish: Iterable[PR] => R
    )
    extends Column[R]
}
