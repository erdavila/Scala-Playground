package mapfoldleft

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

object ScanLeft {
  implicit class Ops[CC[X] <: TraversableLike[X, CC[X]], A](val as: CC[A]) extends AnyVal {
    def mapFoldLeft[
      S,
      B,
      ThatScan,
      That,
    ](z: S)(f: (S, A) => (B, S))(
      implicit
      bf: CanBuildFrom[CC[A], B, That],
      bfScan: CanBuildFrom[CC[A], (S, Option[B]), ThatScan],
      bfMap: CanBuildFrom[ThatScan, B, That],
      ev: ThatScan => TraversableLike[(S, Option[B]), ThatScan],
    ): (S, That) = {
      val scanned = as.scanLeft((z, Option.empty[B])) { case ((s, _), a) =>
        val (b, ss) = f(s, a)
        (ss, Some(b))
      } (bfScan)

      val (finalState, _) = scanned.last
      val bs = scanned.drop(1).collect { case (_, Some(b)) => b } (bfMap)

      (finalState, bs)
    }
  }
}
